{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  Build
-- Description :  Computing, fetching and building plans
--
-- 'computePlan' computes a @cabal@ plan by generating @pkg.cabal@ and
-- @cabal.project@ files with the given dependencies, constraints, flags...,
-- calling @cabal build --dry-run@ to compute a build plan, and parsing
-- the resulting @plan.json@ file.
--
-- 'fetchPlan' calls @cabal unpack@ to fetch all packages in the given plan.
--
-- 'buildPlan' builds each unit in the build plan from source,
-- using 'buildUnit'. This can be done either asynchronously or sequentially
-- in dependency order, depending on the 'BuildStrategy'.
-- 'buildPlan' can instead output a shell script containing build instructions,
-- if using the 'Script' 'BuildStrategy'.
module Build
  ( -- * Computing, fetching and building plans
    computePlan
  , fetchPlan
  , buildPlan

    -- * Generating @pkg.cabal@ and @cabal.project@ files.
  , CabalFilesContents(..)
  , cabalFileContentsFromPackages
  , cabalProjectContentsFromPackages
  ) where

-- base
import Control.Monad.Fix
  ( MonadFix(mfix) )
import Data.Char
  ( isSpace )
import Data.Foldable
  ( for_ )
import Data.Maybe
  ( mapMaybe, maybeToList )
import Data.String
  ( IsString )
import Data.Traversable
  ( for )
import Data.Version
  ( Version )

-- async
import Control.Concurrent.Async
  ( async, wait )

-- bytestring
import qualified Data.ByteString.Lazy as Lazy.ByteString
  ( readFile )

-- containers
import qualified Data.Graph as Graph
  ( graphFromEdges', reverseTopSort )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( (!?), assocs, fromList )
import qualified Data.Map.Lazy as Lazy
  ( Map )
import qualified Data.Map.Lazy as Lazy.Map
  ( fromList )
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( fromList )

-- directory
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing
  , doesDirectoryExist )

-- filepath
import System.FilePath
  ( (</>), (<.>) )

-- text
import Data.Text
  ( Text )
import qualified Data.Text    as Text
  ( all, intercalate, unlines, unpack, unwords )
import qualified Data.Text.IO as Text
  ( writeFile )

-- build-env
import BuildOne
import CabalPlan
import Config
import Script
  ( BuildScript, runBuildScript, script )
import Target
import Utils

--------------------------------------------------------------------------------

-- | The name of the dummy cabal package on which we will call
-- @cabal@ to obtain a build plan.
dummyPackageName :: IsString str => str
dummyPackageName = "build-env-dummy-package"

-- | The 'UnitId' of the (local) dummy package (version 0).
dummyUnitId :: UnitId
dummyUnitId = UnitId $ dummyPackageName <> "-0-inplace"

-- | Query @cabal@ to obtain a build plan for the given packages,
-- by reading the output @plan.json@ of a @cabal build --dry-run@ invocation.
--
-- Use 'generateCabalFilesContents' to generate the @cabal@ file contents
-- from a collection of packages with constraints and flags. See also
-- 'readCabalDotConfig' and 'parseSeedFile' for other ways of obtaining
-- this information.
--
-- Use 'parsePlanBinary' to turn the returned 'CabalPlanBinary' into
-- a 'CabalPlan'.
computePlan :: TempDirPermanence
            -> Verbosity
            -> Compiler
            -> Cabal
            -> CabalFilesContents
            -> IO CabalPlanBinary
computePlan delTemp verbosity comp cabal ( CabalFilesContents { cabalContents, projectContents } ) =
  withTempDir delTemp "build" \ dir -> do
    verboseMsg verbosity $ "Computing plan in build directory " ++ dir
    Text.writeFile (dir </> "cabal" <.> "project") projectContents
    Text.writeFile (dir </> dummyPackageName <.> "cabal") cabalContents
    let cabalBuildArgs =
          globalCabalArgs cabal ++
            [ "build"
            , "--dry-run"
            , "--with-compiler", ghcPath comp
            , cabalVerbosity verbosity ]
    debugMsg verbosity $
      unlines $ "cabal" : map ("  " <>) cabalBuildArgs
    callProcessInIO $
      CP { cwd          = dir
         , prog         = cabalPath cabal
         , args         = cabalBuildArgs
         , extraPATH    = []
         , extraEnvVars = [] }

    let planPath = dir </> "dist-newstyle" </> "cache" </> "plan.json"
    CabalPlanBinary <$> Lazy.ByteString.readFile planPath

-- | The contents of a dummy @cabal.project@ file, specifying
-- package constraints, flags and allow-newer.
cabalProjectContentsFromPackages :: PkgSpecs -> AllowNewer -> Text
cabalProjectContentsFromPackages allPkgs (AllowNewer allowNewer) =
     "packages: .\n\n"
   <> allowNewers
   <> flagSpecs
   <> constraints
  where
    constraints = Text.unlines
        [ Text.unwords ["constraints:", unPkgName nm, cts]
        | (nm, ps) <- Map.assocs allPkgs
        , Constraints cts <- maybeToList $ psConstraints ps
        , not (Text.all isSpace cts)
        ]

    allowNewers
      | null allowNewer
      = ""
      | otherwise
      = Text.unlines $
          "allow-newer:" :
            [ "    " <> p <> ":" <> q <> ","
            | (p,q) <- allowNewer ]

    flagSpecs = Text.unlines
        [ Text.unlines
          [ "package " <> unPkgName nm
          , "  flags: " <> showFlagSpec (psFlags ps)
          ]
        | (nm, ps) <- Map.assocs allPkgs
        , let flags = psFlags ps
        , not $ flagSpecIsEmpty flags
        ]

-- | The contents of a dummy @cabal@ file with dependencies on
-- the specified packages (without any constraints).
--
-- The corresponding package Id is 'dummyPackageId'.
cabalFileContentsFromPackages :: [PkgName] -- ^ libraries
                              -> [PkgName] -- ^ executables
                              -> Text
cabalFileContentsFromPackages libs exes =
  Text.unlines
    [ "cabal-version: 2.4"
    , "name: " <> dummyPackageName
    , "version: 0"
    , "library"
    , "  build-depends:"
    ] <> Text.intercalate ",\n"
         [ "    " <> nm
         | PkgName nm <- libs ]
     <> if null exes
        then ""
        else "\n  build-tool-depends:\n"
          <> Text.intercalate ",\n"
              [ "    " <> nm <> ":" <> nm
              | PkgName nm <- exes ]

-- | The file contents of the cabal files of a cabal project;
-- @pkg.cabal@ and @cabal.project@.
data CabalFilesContents
  = CabalFilesContents
    { cabalContents   :: Text
      -- ^ The package @.cabal@ file contents.
    , projectContents :: Text
      -- ^ The @cabal.project@ file contents.
    }

-- | Fetch the sources of a 'CabalPlan', calling @cabal unpack@ on each
-- package and putting it into the correspondingly named and versioned
-- subfolder of the specified directory (e.g. @pkg-name-1.2.3@).
fetchPlan :: Verbosity
          -> Cabal
          -> FilePath  -- ^ directory in which to put the sources
          -> CabalPlan
          -> IO ()
fetchPlan verbosity cabal fetchDir cabalPlan =
    for_ pkgs \ (pkgNm, pkgVer) -> do
      let nameVersion = Text.unpack $ pkgNameVersion pkgNm pkgVer
      pkgDirExists <- doesDirectoryExist (fetchDir </> nameVersion)
      if   pkgDirExists
      then normalMsg verbosity $ "NOT fetching " <> nameVersion
      else cabalFetch verbosity cabal fetchDir nameVersion
  where
    pkgs :: Set (PkgName, Version)
    pkgs = Set.fromList
               -- Some packages might have multiple components;
               -- we don't want to fetch the package itself multiple times.
         $ mapMaybe relevantPkgNameVersion
         $ planUnits cabalPlan

    relevantPkgNameVersion :: PlanUnit -> Maybe (PkgName, Version)
    relevantPkgNameVersion = \case
      PU_Configured ( ConfiguredUnit { puId = unitId, puPkgName = nm, puVersion = ver } )
        | unitId /= dummyUnitId
        -> Just (nm, ver)
      _ -> Nothing

-- | Call @cabal get@ to fetch a single package from Hackage.
cabalFetch :: Verbosity -> Cabal -> FilePath -> String -> IO ()
cabalFetch verbosity cabal root pkgNmVer = do
    normalMsg verbosity $ "Fetching " <> pkgNmVer
    let args = globalCabalArgs cabal ++
                 [ "get"
                 , pkgNmVer
                 , cabalVerbosity verbosity ]
    callProcessInIO $
      CP { cwd          = root
         , prog         = cabalPath cabal
         , args
         , extraPATH    = []
         , extraEnvVars = [] }


-- | Sort the units in a 'CabalPlan' in dependency order.
sortPlan :: CabalPlan -> [ConfiguredUnit]
sortPlan plan =
    map (fst3 . lookupVertex) $ Graph.reverseTopSort gr
  where
    fst3 :: (a,b,c) -> a
    fst3 (a,_,_) = a
    (gr, lookupVertex) = Graph.graphFromEdges'
       [ (pu, puId, allDepends pu)
       | PU_Configured pu@(ConfiguredUnit { puId }) <- planUnits plan
       ]

-- | Build a 'CabalPlan'. This will install all the packages in the plan
-- by running their @Setup.hs@ scripts, and then register them
-- into a local package database at @installdir/package.conf@.
--
-- Note: this function will fail if one of the packages has already been
-- registed in the package database.
buildPlan :: Verbosity
          -> Compiler
          -> FilePath    -- ^ fetched sources directory (input)
          -> DestDir Raw -- ^ installation directory structure (output)
          -> BuildStrategy
          -> TargetArgs  -- ^ extra @Setup configure@ arguments
                         -- (use this to specify haddock, hsc2hs, etc)
          -> Args        -- ^ extra @ghc-pkg@ arguments
          -> CabalPlan   -- ^ the build plan to execute
          -> IO ()
buildPlan verbosity comp fetchDir0 destDir0
          buildStrat
          configureArgs ghcPkgArgs
          cabalPlan
  = do
    fetchDir <- canonicalizePath fetchDir0
    dest@( DestDir { installDir, prefix, destDir } )
      <- canonicalizeDestDir destDir0
    createDirectoryIfMissing True installDir

    verboseMsg verbosity $
      unlines [ "       prefix: " <> prefix
              , "      destDir: " <> destDir
              , "   installDir: " <> installDir ]

    let prepareUnit :: ConfiguredUnit -> IO BuildScript
        prepareUnit pu@(ConfiguredUnit { puPkgName, puComponentName }) = do
          let srcDir = fetchDir
              pkgConfigureArgs =
                lookupTargetArgs configureArgs puPkgName puComponentName
          buildUnit verbosity comp srcDir dest
            pkgConfigureArgs ghcPkgArgs
            depMap pu

    debugMsg verbosity $
      "Units to build: " <>
        unlines
          ( map
            ( Text.unpack . cabalComponent . puComponentName )
            unitsToBuild
          )

    case buildStrat of
      Async -> do
        -- First prepare all the units.
        let prepUnit cu = do { steps <- prepareUnit cu; return (cu, steps) }
        buildScripts <- for unitMap prepUnit

        -- Then compile them asynchonously.
        unitAsyncs <- mfix \ unitAsyncs ->
          let doPkgAsync :: (ConfiguredUnit, BuildScript) -> IO ()
              doPkgAsync (pu, steps) = do
                  for_ (allDepends pu) \ depUnitId ->
                    for_ (unitAsyncs Map.!? depUnitId) \ cu ->
                      -- (Nothing for Preexisting packages)
                      wait cu
                  runBuildScript steps
           in traverse (async . doPkgAsync) buildScripts
        mapM_ wait unitAsyncs

      _ -> do
        buildScripts <- for unitsToBuild prepareUnit
        case buildStrat of
          TopoSort ->
            for_ buildScripts runBuildScript
          Script fp ->
            Text.writeFile fp (script $ concat buildScripts)

  where

    unitsToBuild :: [ConfiguredUnit]
    unitsToBuild
      = filter (\ ( ConfiguredUnit { puId } ) -> puId /= dummyUnitId )
      $ sortPlan cabalPlan

    unitMap :: Lazy.Map UnitId ConfiguredUnit
    unitMap = Lazy.Map.fromList
              [ (puId, pu)
              | pu@( ConfiguredUnit { puId } ) <- unitsToBuild ]

    depMap :: Map UnitId PlanUnit
    depMap = Map.fromList
              [ (planUnitUnitId pu, pu)
              | pu <- planUnits cabalPlan ]
