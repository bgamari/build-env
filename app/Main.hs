{-# LANGUAGE DataKinds #-}

module Main ( main ) where

-- base
import Control.Monad
  ( guard )
import Data.Foldable
  ( for_ )
import System.IO
  ( BufferMode(..), hSetBuffering, stdout )

-- bytestring
import qualified Data.ByteString.Lazy as Lazy.ByteString
  ( readFile, writeFile )

-- containers
import qualified Data.Map as Map
  ( empty )
import qualified Data.Set as Set
  ( empty, member )

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
import BuildEnv.Path

--------------------------------------------------------------------------------

main :: IO ()
main = do
  ( GlobalOpts
      { compiler, cabal
      , verbosity, delTemp, workDir
      , indexState = mbIndexState }
    , mode )
    <- runOptionsParser
  hSetBuffering stdout $ BlockBuffering Nothing
  case mode of
    PlanMode { planModeInputs, planOutput } -> do
      CabalPlanBinary planBinary <-
        computePlanFromInputs
          delTemp verbosity workDir compiler cabal
          mbIndexState planModeInputs
      normalMsg verbosity $
        "Writing build plan to" <> Text.pack ( show planOutput )
      Lazy.ByteString.writeFile ( interpretSymbolicPath workDir planOutput ) planBinary
    FetchMode ( FetchDescription { fetchDir, fetchInputPlan } ) newOrUpd -> do
      plan <- getPlan delTemp verbosity workDir compiler cabal mbIndexState fetchInputPlan
      doFetch verbosity cabal workDir fetchDir mbIndexState True newOrUpd plan
    BuildMode ( Build { buildBuildPlan
                      , buildStart
                      , buildStrategy
                      , buildRawPaths = rawPaths
                      , mbOnlyDepsOf
                      , eventLogDir
                      , userUnitArgs } ) -> do
      plan <- getPlan delTemp verbosity workDir compiler cabal mbIndexState buildBuildPlan
      ( pathsForPrep@( Paths { fetchDir }), pathsForBuild )
        <- canonicalizePaths compiler buildStrategy workDir rawPaths
      case buildStart of
        Fetch newOrUpd -> doFetch verbosity cabal workDir fetchDir mbIndexState False newOrUpd plan
        _              -> return ()
      let mbOnlyDepsOfUnits :: Maybe [UnitId]
          mbOnlyDepsOfUnits =
            case mbOnlyDepsOf of
              Nothing -> Nothing
              Just pkgs ->
                let wantUnit :: PlanUnit -> Maybe UnitId
                    wantUnit pu = do
                      ConfiguredUnit { puPkgName, puId } <- configuredUnitMaybe pu
                      guard ( puPkgName `Set.member` pkgs )
                      return puId
                in Just $ mapMaybePlanUnits wantUnit plan

      let resumeBuild = case buildStart of { Resume -> True; _ -> False }
      buildPlan verbosity workDir pathsForPrep pathsForBuild eventLogDir
        buildStrategy resumeBuild mbOnlyDepsOfUnits userUnitArgs plan

-- | Generate the contents of @pkg.cabal@ and @cabal.project@ files, using
--
--  - a seed file containing packages to build (with constraints, flags
--    and allow-newer),
--  - a @cabal.config@ freeze file,
--  - explicit packages and allow-newer specified as command-line arguments.
parsePlanInputs :: Verbosity
                -> SymbolicPath CWD ( Dir Project )
                -> Maybe IndexState -> PlanInputs -> IO CabalFilesContents
parsePlanInputs verbosity workDir mbIndexState
  ( PlanInputs { planPins, planUnits, planAllowNewer } )
  = do
      ( pkgs, fileAllowNewer ) <- parsePlanUnits verbosity workDir planUnits
      let
        allAllowNewer = fileAllowNewer <> planAllowNewer
          -- NB: allow-newer specified in the command-line overrides
          -- the allow-newer included in the seed file.
        cabalContents = cabalFileContentsFromPackages pkgs
      pins <-
        case planPins of
          Nothing -> return Map.empty
          Just ( FromFile pinCabalConfig ) -> do
            normalMsg verbosity $
              "Reading 'cabal.config' file at " <> Text.pack ( show pinCabalConfig )
            pins <- parseCabalDotConfigPkgs ( interpretSymbolicPath workDir pinCabalConfig )
            return pins
          Just ( Explicit pins ) -> do
            return pins
      projectContents <-
        cabalProjectContentsFromPackages workDir pkgs pins allAllowNewer mbIndexState
      return $ CabalFilesContents { cabalContents, projectContents }

-- | Retrieve the seed units we want to build, either from a seed file
-- or from explicit command line arguments.
parsePlanUnits :: Verbosity
               -> SymbolicPath CWD ( Dir Project )
               -> PackageData UnitSpecs -> IO ( UnitSpecs, AllowNewer )
parsePlanUnits _ _ ( Explicit units ) = return (units, AllowNewer Set.empty)
parsePlanUnits verbosity workDir ( FromFile fp ) = do
  normalMsg verbosity $
    "Reading seed packages from " <> Text.pack ( show fp ) <> ""
  parseSeedFile ( interpretSymbolicPath workDir fp )

-- | Compute a build plan by calling @cabal build --dry-run@ with the generated
-- @pkg.cabal@ and @cabal.project@ files.
computePlanFromInputs :: TempDirPermanence
                      -> Verbosity
                      -> SymbolicPath CWD ( Dir Project )
                      -> Compiler
                      -> Cabal
                      -> Maybe IndexState
                      -> PlanInputs
                      -> IO CabalPlanBinary
computePlanFromInputs delTemp verbosity workDir comp cabal mbIndexState inputs = do
  cabalFileContents <- parsePlanInputs verbosity workDir mbIndexState inputs
  normalMsg verbosity "Computing build plan"
  computePlan delTemp verbosity comp cabal workDir cabalFileContents

-- | Retrieve a cabal build plan, either by computing it or using
-- a pre-existing @plan.json@ file.
getPlan :: TempDirPermanence -> Verbosity
        -> SymbolicPath CWD ( Dir Project )
        -> Compiler -> Cabal -> Maybe IndexState -> Plan -> IO CabalPlan
getPlan delTemp verbosity workDir comp cabal mbIndexState planMode = do
  planBinary <-
    case planMode of
      ComputePlan planInputs mbPlanOutputPath -> do
        plan@( CabalPlanBinary planData ) <-
          computePlanFromInputs delTemp verbosity workDir comp cabal mbIndexState planInputs
        for_ mbPlanOutputPath \ planOutputPath ->
          Lazy.ByteString.writeFile ( interpretSymbolicPath workDir planOutputPath ) planData
        return plan
      UsePlan { planJSONPath } -> do
        normalMsg verbosity $
          "Reading build plan from " <> Text.pack ( show planJSONPath )
        CabalPlanBinary <$> Lazy.ByteString.readFile ( interpretSymbolicPath workDir planJSONPath )
  return $ parsePlanBinary planBinary

-- | Fetch all packages in a cabal build plan.
doFetch :: Verbosity
        -> Cabal
        -> SymbolicPath CWD ( Dir Project )
           -- ^ working directory
        -> SymbolicPath Project ( Dir Fetch )
           -- ^ fetch directory
        -> Maybe IndexState
        -> Bool -- ^ True <=> we are fetching (not building)
                -- (only relevant for error messages)
        -> NewOrExisting
        -> CabalPlan
        -> IO ()
doFetch verbosity cabal workDir fetchDir mbIndexState weAreFetching newOrUpd plan = do
  fetchDirExists <- doesDirectoryExist $ interpretSymbolicPath workDir fetchDir
  case newOrUpd of
    New | fetchDirExists ->
      error $ unlines $
       "Fetch directory already exists." : existsMsg
          ++ [ "Fetch directory: " <> show fetchDir ]
    Existing | not fetchDirExists ->
      error $ unlines
        [ "Fetch directory must already exist when using --update."
        , "Fetch directory: " <> show fetchDir ]
    _ -> return ()
  createDirectoryIfMissing True $ interpretSymbolicPath workDir fetchDir
  normalMsg verbosity $
    "Fetching sources from build plan into directory " <> Text.pack ( show fetchDir )
  fetchPlan verbosity cabal workDir mbIndexState fetchDir plan

  where
    existsMsg
      | weAreFetching
      = [ "Use --update to update an existing directory." ]
      | otherwise
      = [ "Use --prefetched to build using a prefetched source directory,"
        , "or --update to continue fetching before building." ]
