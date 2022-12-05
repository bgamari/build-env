module Main ( main ) where

-- bytestring
import qualified Data.ByteString.Lazy as BSL
  ( readFile, writeFile )

-- containers
import qualified Data.Map as Map
  ( keys, union )

-- directory
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing
  , doesDirectoryExist )

-- build-env
import Build
import CabalPlan
import Config
import File
  ( parseCabalDotConfigPkgs, parseSeedFile )
import Options
import Parse
  ( runOptionsParser )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  Opts { compiler, cabal, mode, verbosity, delTemp } <- runOptionsParser
  case mode of
    PlanMode { planModeInputs, planOutput } -> do
      CabalPlanBinary planBinary <-
        computePlanFromInputs delTemp verbosity compiler cabal planModeInputs
      normalMsg verbosity $
        "Writing build plan to '" <> planOutput <> "'"
      BSL.writeFile planOutput planBinary
    FetchMode ( FetchDescription { fetchDir, fetchInputPlan } ) newOrUpd -> do
      plan <- getPlan delTemp verbosity compiler cabal fetchInputPlan
      doFetch verbosity cabal fetchDir newOrUpd plan
    BuildMode ( Build { buildFetchDescr = FetchDescription { fetchDir, fetchInputPlan }
                      , buildFetch, buildStrategy, buildDestDir
                      , configureArgs, ghcPkgArgs } ) -> do
      plan <- getPlan delTemp verbosity compiler cabal fetchInputPlan
      case buildFetch of
        Prefetched     -> return ()
        Fetch newOrUpd -> doFetch verbosity cabal fetchDir newOrUpd plan
      normalMsg verbosity "Building and registering packages"
      buildPlan delTemp verbosity compiler fetchDir buildDestDir buildStrategy
        configureArgs ghcPkgArgs
        plan

-- | Generate the contents of @pkg.cabal@ and @cabal.project@ files, using
--
--  - a seed file containing packages to build (with constraints, flags
--    and allow-newer),
--  - a @cabal.config@ freeze file,
--  - explicit packages and allow-newer specified as command-line arguments.
parsePlanInputs :: Verbosity -> PlanInputs -> IO CabalFilesContents
parsePlanInputs verbosity (PlanInputs { planPins, planPkgs, planAllowNewer })
  = do (libs, exes, fileAllowNewer) <- parsePlanPackages verbosity planPkgs
       let
         allAllowNewer = fileAllowNewer <> planAllowNewer
           -- NB: allow-newer specified in the command-line overrides
           -- the allow-newer included in the seed file.
         pkgs = libs `Map.union` exes
         cabalContents = cabalFileContentsFromPackages (Map.keys libs) (Map.keys exes)
       projectContents <-
         case planPins of
           Nothing -> return $ cabalProjectContentsFromPackages pkgs allAllowNewer
           Just (FromFile pinCabalConfig) -> do
             normalMsg verbosity $
               "Reading 'cabal.config' file at '" <> pinCabalConfig <> "'"
             pins <- parseCabalDotConfigPkgs pinCabalConfig
             return $
               cabalProjectContentsFromPackages
                 (pkgs `unionPkgSpecs` pins)
                   -- NB: unionPkgsSpecs is left-biased: constraints from the
                   -- SEED file override constraints from the cabal.config file.
                 allAllowNewer
           Just (Explicit pinnedLibs pinnedExes) -> do
             let allPkgs = pkgs `Map.union` pinnedLibs `Map.union` pinnedExes
             return $ cabalProjectContentsFromPackages allPkgs allAllowNewer
       return $ CabalFilesContents { cabalContents, projectContents }

-- | Retrieve the seed packages we want to build, either from a seed file
-- or from explicit command line arguments.
parsePlanPackages :: Verbosity -> PackageData -> IO (PkgSpecs, PkgSpecs, AllowNewer)
parsePlanPackages _ (Explicit libs exes) = return (libs, exes, AllowNewer [])
parsePlanPackages verbosity (FromFile fp) =
  do normalMsg verbosity $
       "Reading seed packages from '" <> fp <> "'"
     parseSeedFile fp

-- | Compute a build plan by calling @cabal build --dry-run@ with the generated
-- @pkg.cabal@ and @cabal.project@ files.
computePlanFromInputs :: TempDirPermanence
                      -> Verbosity
                      -> Compiler
                      -> Cabal
                      -> PlanInputs
                      -> IO CabalPlanBinary
computePlanFromInputs delTemp verbosity comp cabal inputs
    = do cabalFileContents <- parsePlanInputs verbosity inputs
         normalMsg verbosity "Computing build plan"
         computePlan delTemp verbosity comp cabal cabalFileContents

-- | Retrieve a cabal build plan, either by computing it or using
-- a pre-existing @plan.json@ file.
getPlan :: TempDirPermanence -> Verbosity -> Compiler -> Cabal -> Plan -> IO CabalPlan
getPlan delTemp verbosity comp cabal planMode = do
   planBinary <-
     case planMode of
       ComputePlan planInputs   ->
        computePlanFromInputs delTemp verbosity comp cabal planInputs
       UsePlan { planJSONPath } ->
         do
           normalMsg verbosity $
             "Reading build plan from '" <> planJSONPath <> "'"
           CabalPlanBinary <$> BSL.readFile planJSONPath
   return $ parsePlanBinary planBinary

-- | Fetch all packages in a cabal build plan.
doFetch :: Verbosity -> Cabal -> FilePath -> NewOrExisting -> CabalPlan -> IO ()
doFetch verbosity cabal fetchDir0 newOrUpd plan = do
  fetchDir       <- canonicalizePath fetchDir0
  fetchDirExists <- doesDirectoryExist fetchDir
  case newOrUpd of
    New | fetchDirExists ->
      error $ unlines
        [ "Fetch directory already exists."
        , "Use --update to update an existing directory."
        , "Fetch directory: " <> fetchDir ]
    Existing | not fetchDirExists ->
      error $ unlines
        [ "Fetch directory must already exist when using --update."
        , "Fetch directory: " <> fetchDir ]
    _ -> return ()
  createDirectoryIfMissing True fetchDir
  normalMsg verbosity $
    "Fetching sources from build plan into directory '" <> fetchDir <> "'"
  fetchPlan verbosity cabal fetchDir plan
