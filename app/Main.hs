module Main ( main ) where

-- base
import Data.Foldable
  ( for_ )

-- bytestring
import qualified Data.ByteString.Lazy as Lazy.ByteString
  ( readFile, writeFile )

-- containers
import qualified Data.Map as Map
  ( empty )
import qualified Data.Set as Set
  ( empty )

-- directory
import System.Directory

-- text
import qualified Data.Text as Text
  ( pack )

-- build-env
import BuildEnv.Build
import BuildEnv.CabalPlan
import BuildEnv.Config
import BuildEnv.File
  ( parseCabalDotConfigPkgs, parseSeedFile )
import BuildEnv.Options
import BuildEnv.Parse
  ( runOptionsParser )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  Opts { compiler, cabal, mode, verbosity, delTemp, workDir }
    <- runOptionsParser currentDir
  withCurrentDirectory workDir $
    case mode of
      PlanMode { planModeInputs, planOutput } -> do
        CabalPlanBinary planBinary <-
          computePlanFromInputs delTemp verbosity workDir compiler cabal planModeInputs
        normalMsg verbosity $
          "Writing build plan to '" <> Text.pack planOutput <> "'"
        Lazy.ByteString.writeFile planOutput planBinary
      FetchMode ( FetchDescription { fetchDir, fetchInputPlan } ) newOrUpd -> do
        plan <- getPlan delTemp verbosity workDir compiler cabal fetchInputPlan
        doFetch verbosity cabal fetchDir True newOrUpd plan
      BuildMode ( Build { buildBuildPlan
                        , buildFetch, buildStrategy
                        , buildDirs = buildDirs@( Dirs { fetchDir } )
                        , userUnitArgs } ) -> do
        plan <- getPlan delTemp verbosity workDir compiler cabal buildBuildPlan
        case buildFetch of
          Prefetched     -> return ()
          Fetch newOrUpd -> doFetch verbosity cabal fetchDir False newOrUpd plan
        buildPlan verbosity compiler workDir buildDirs buildStrategy
          userUnitArgs plan

-- | Generate the contents of @pkg.cabal@ and @cabal.project@ files, using
--
--  - a seed file containing packages to build (with constraints, flags
--    and allow-newer),
--  - a @cabal.config@ freeze file,
--  - explicit packages and allow-newer specified as command-line arguments.
parsePlanInputs :: Verbosity -> FilePath -> PlanInputs -> IO CabalFilesContents
parsePlanInputs verbosity workDir (PlanInputs { planPins, planUnits, planAllowNewer })
  = do (pkgs, fileAllowNewer) <- parsePlanUnits verbosity planUnits
       let
         allAllowNewer = fileAllowNewer <> planAllowNewer
           -- NB: allow-newer specified in the command-line overrides
           -- the allow-newer included in the seed file.
         cabalContents = cabalFileContentsFromPackages pkgs
       projectContents <-
         case planPins of
           Nothing -> return $ cabalProjectContentsFromPackages workDir pkgs Map.empty allAllowNewer
           Just (FromFile pinCabalConfig) -> do
             normalMsg verbosity $
               "Reading 'cabal.config' file at '" <> Text.pack pinCabalConfig <> "'"
             pins <- parseCabalDotConfigPkgs pinCabalConfig
             return $ cabalProjectContentsFromPackages workDir pkgs pins allAllowNewer
           Just (Explicit pins) -> do
             return $ cabalProjectContentsFromPackages workDir pkgs pins allAllowNewer
       return $ CabalFilesContents { cabalContents, projectContents }

-- | Retrieve the seed units we want to build, either from a seed file
-- or from explicit command line arguments.
parsePlanUnits :: Verbosity -> PackageData UnitSpecs -> IO (UnitSpecs, AllowNewer)
parsePlanUnits _ (Explicit units) = return (units, AllowNewer Set.empty)
parsePlanUnits verbosity (FromFile fp) =
  do normalMsg verbosity $
       "Reading seed packages from '" <> Text.pack fp <> "'"
     parseSeedFile fp

-- | Compute a build plan by calling @cabal build --dry-run@ with the generated
-- @pkg.cabal@ and @cabal.project@ files.
computePlanFromInputs :: TempDirPermanence
                      -> Verbosity
                      -> FilePath
                      -> Compiler
                      -> Cabal
                      -> PlanInputs
                      -> IO CabalPlanBinary
computePlanFromInputs delTemp verbosity workDir comp cabal inputs
    = do cabalFileContents <- parsePlanInputs verbosity workDir inputs
         normalMsg verbosity "Computing build plan"
         computePlan delTemp verbosity comp cabal cabalFileContents

-- | Retrieve a cabal build plan, either by computing it or using
-- a pre-existing @plan.json@ file.
getPlan :: TempDirPermanence -> Verbosity -> FilePath -> Compiler -> Cabal -> Plan -> IO CabalPlan
getPlan delTemp verbosity workDir comp cabal planMode = do
   planBinary <-
     case planMode of
       ComputePlan planInputs mbPlanOutputPath -> do
        plan@(CabalPlanBinary planData) <-
          computePlanFromInputs delTemp verbosity workDir comp cabal planInputs
        for_ mbPlanOutputPath \ planOutputPath ->
          Lazy.ByteString.writeFile planOutputPath planData
        return plan
       UsePlan { planJSONPath } ->
         do
           normalMsg verbosity $
             "Reading build plan from '" <> Text.pack planJSONPath <> "'"
           CabalPlanBinary <$> Lazy.ByteString.readFile planJSONPath
   return $ parsePlanBinary planBinary

-- | Fetch all packages in a cabal build plan.
doFetch :: Verbosity
        -> Cabal
        -> FilePath
        -> Bool -- ^ True <=> we are fetching (not building)
                -- (only relevant for error messages)
        -> NewOrExisting
        -> CabalPlan
        -> IO ()
doFetch verbosity cabal fetchDir0 weAreFetching newOrUpd plan = do
  fetchDir       <- canonicalizePath fetchDir0
  fetchDirExists <- doesDirectoryExist fetchDir
  case newOrUpd of
    New | fetchDirExists ->
      error $ unlines $
       "Fetch directory already exists." : existsMsg
          ++ [ "Fetch directory: " <> fetchDir  ]
    Existing | not fetchDirExists ->
      error $ unlines
        [ "Fetch directory must already exist when using --update."
        , "Fetch directory: " <> fetchDir ]
    _ -> return ()
  createDirectoryIfMissing True fetchDir
  normalMsg verbosity $
    "Fetching sources from build plan into directory '" <> Text.pack fetchDir <> "'"
  fetchPlan verbosity cabal fetchDir plan

  where
    existsMsg
      | weAreFetching
      = [ "Use --update to update an existing directory." ]
      | otherwise
      = [ "Use --prefetched to build using a prefetched source directory,"
        , "or --update to continue fetching before building." ]
