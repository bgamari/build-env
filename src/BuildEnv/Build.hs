{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  BuildEnv.Build
-- Description :  Computing, fetching and building plans
--
-- 'computePlan' computes a Cabal plan by generating @pkg.cabal@ and
-- @cabal.project@ files with the given dependencies, constraints, flags...,
-- calling @cabal build --dry-run@ to compute a build plan, and parsing
-- the resulting @plan.json@ file.
--
-- 'fetchPlan' calls @cabal unpack@ to fetch all packages in the given plan.
--
-- 'buildPlan' builds each unit in the build plan from source,
-- using 'buildUnit'. This can be done either asynchronously or sequentially
-- in dependency order, depending on the 'BuildStrategy'.
-- 'buildPlan' can also be used to output a shell script containing
-- build instructions, with the 'Script' 'BuildStrategy'.
module BuildEnv.Build
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
import Control.Exception
  ( IOException, catch )
import Control.Monad
  ( when )
import Control.Monad.Fix
  ( MonadFix(mfix) )
import Data.Char
  ( isSpace )
import Data.Foldable
  ( for_, toList )
import Data.IORef
  ( newIORef )
import Data.Functor
  ( (<&>) )
import Data.Maybe
  ( catMaybes, mapMaybe, maybeToList, isNothing )
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
  ( dfs, graphFromEdges, reverseTopSort )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as Lazy
  ( Map )
import qualified Data.Map.Lazy as Lazy.Map
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( elems, fromList, member, toList )

-- directory
import System.Directory
  ( doesDirectoryExist, doesFileExist
  , exeExtension, listDirectory
  , removeDirectoryRecursive
  )

-- process
import qualified System.Process as Process
  ( readProcess )

-- text
import Data.Text
  ( Text )
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
  ( writeFile )

-- build-env
import BuildEnv.BuildOne
  ( PkgDbDir(..)
  , getPkgDbDirForPrep, getPkgDbDirForBuild
  , getPkgDir
  , setupPackage, buildUnit
  )
import BuildEnv.CabalPlan
import qualified BuildEnv.CabalPlan as Configured
  ( ConfiguredUnit(..) )
import BuildEnv.Config
import BuildEnv.Ninja
  ( ninja )
import BuildEnv.Script
  ( BuildScript, BuildSteps, ScriptOutput(..), ScriptConfig(..)
  , buildSteps, executeBuildScript, executeBuildScriptNoProcessEnv
  , shellScriptHeader, shellScriptSteps, createDir, logMessage
  )
import BuildEnv.Utils
  ( Program(..), CallProcess(..), callProcessInIO, withTempDir
  , AbstractSem(..), withNewAbstractSem
  )
import BuildEnv.Path
import Data.Either (fromRight)

--------------------------------------------------------------------------------
-- Planning.

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
-- Use 'cabalFileContentsFromPackages' and 'cabalProjectContentsFromPackages'
-- to generate the @cabal@ file contents from a collection of packages with
-- constraints and flags.
-- See also 'BuildEnv.File.parseCabalDotConfigPkgs' and
-- 'BuildEnv.File.parseSeedFile' for other ways of obtaining this information.
--
-- Use 'parsePlanBinary' to convert the returned 'CabalPlanBinary' into
-- a 'CabalPlan'.
computePlan :: TempDirPermanence
            -> Verbosity
            -> Compiler
            -> Cabal
            -> SymbolicPath CWD ( Dir Project )
            -> CabalFilesContents
            -> IO CabalPlanBinary
computePlan delTemp verbosity comp cabal _workDir ( CabalFilesContents { cabalContents, projectContents } ) =
  withTempDir delTemp "build" \ tmpDir -> do
    verboseMsg verbosity $ "Computing plan in build directory " <> Text.pack ( show tmpDir )
    Text.writeFile (getAbsolutePath tmpDir </> "cabal" <.> "project") projectContents
    Text.writeFile (getAbsolutePath tmpDir </> dummyPackageName <.> "cabal") cabalContents
    let cabalBuildArgs =
          globalCabalArgs cabal ++
            [ "build"
            , "--dry-run"
            , "--with-compiler", getAbsolutePath ( ghcPath comp )
            , cabalVerbosity verbosity ]
        procEnv =
          ProcessEnv
            { cwd          = absoluteSymbolicPath tmpDir
            , extraPATH    = []
            , extraEnvVars = []
            , logBasePath  = Nothing
            }
    debugMsg verbosity $
      Text.unlines $ "cabal" : map ( ( "  " <> ) . Text.pack ) cabalBuildArgs
    callProcessInIO @Tmp comp cabal procEnv Nothing $
      CP { prog     = CabalProgram
         , args     = cabalBuildArgs
         , lockFile = Nothing
         }

    let planPath :: FilePath
        planPath = getAbsolutePath tmpDir </> "dist-newstyle" </> "cache" </> "plan.json"
    CabalPlanBinary <$> Lazy.ByteString.readFile planPath

-- | The contents of a dummy @cabal.project@ file, specifying
-- package constraints, flags and allow-newer.
cabalProjectContentsFromPackages
  :: SymbolicPath CWD ( Dir Project )
  -> UnitSpecs
  -> PkgSpecs
  -> AllowNewer
  -> Maybe IndexState
  -> IO Text
cabalProjectContentsFromPackages workDir units pins ( AllowNewer allowNewer ) mbIndexState = do

  -- Make all the local package paths into absolute paths, as we are
  -- putting the cabal.project file off in some temporary directory.
  ( localPkgs :: [ AbsolutePath ( Dir Pkg ) ] )
    <- traverse ( makeAbsolute workDir ) $ mapMaybe isLocal ( Map.elems units )

  let
    packages
      | null localPkgs
      = "packages: .\n\n"
      | otherwise
      = Text.intercalate ",\n          "
        ( "packages: ." : map ( Text.pack . getAbsolutePath ) localPkgs )
      <> "\n"

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
            | (p,q) <- Set.elems allowNewer ]

    flagSpecs = Text.unlines
        [ Text.unlines
          [ "package " <> unPkgName nm
          , "  flags: " <> showFlagSpec (psFlags ps)
          ]
        | (nm, ps) <- Map.assocs allPkgs
        , let flags = psFlags ps
        , not $ flagSpecIsEmpty flags
        ]

    indexStateDecl = case mbIndexState of
      Nothing -> ""
      Just ( IndexState date ) ->
        Text.unlines [ "index-state: " <> date ]

  return $
        packages
     <> allowNewers
     <> flagSpecs
     <> constraints
     <> indexStateDecl

  where

    isLocal :: ( PkgSrc, PkgSpec, Set ComponentName ) -> Maybe ( SymbolicPath Project ( Dir Pkg ) )
    isLocal ( Local src, _, _ ) = Just src
    isLocal _ = Nothing

    allPkgs :: PkgSpecs
    allPkgs = fmap ( \ ( _, spec, _ ) -> spec ) units
            `unionPkgSpecsOverriding`
              pins
      -- Constraints from the SEED file (units) should override
      -- constraints from the cabal.config file (pins).

-- | The contents of a dummy Cabal file with dependencies on
-- the specified units (without any constraints).
cabalFileContentsFromPackages :: UnitSpecs -> Text
cabalFileContentsFromPackages units =
  Text.unlines
    [ "cabal-version: 3.0"
    , "name: " <> dummyPackageName
    , "version: 0"
    , "library" ]
  <> libDepends
  <> exeDepends
  where
    isLib (ComponentName ty lib) = case ty of { Lib -> Just lib; _ -> Nothing }
    isExe (ComponentName ty exe) = case ty of { Exe -> Just exe; _ -> Nothing }
    allLibs = [ (pkg, libsInPkg)
              | (pkg, (_, _, comps)) <- Map.assocs units
              , let libsInPkg = mapMaybe isLib $ Set.toList comps
              , not (null libsInPkg) ]
    allExes = [ (pkg, exesInPkg)
            | (pkg, (_, _, comps)) <- Map.assocs units
            , let exesInPkg = mapMaybe isExe $ Set.toList comps
            , not (null exesInPkg) ]

    dep (PkgName pkg) [comp]
      = pkg <> ":" <> comp
    dep (PkgName pkg) comps
      = pkg <> ":{" <> Text.intercalate "," comps <> "}"

    libDepends
      | null allLibs
      = ""
      | otherwise
      = "\n  build-depends:\n"
          <> Text.intercalate ",\n"
               [ "    " <> dep pkg libs
               | (pkg, libs) <- allLibs ]
      <> "\n"

    exeDepends
      | null allExes
      = ""
      | otherwise
      = "\n  build-tool-depends:\n"
          <> Text.intercalate ",\n"
               [ "    " <> dep pkg exes
               | (pkg, exes) <- allExes ]
      <> "\n"

-- | The file contents of the Cabal files of a Cabal project:
-- @pkg.cabal@ and @cabal.project@.
data CabalFilesContents
  = CabalFilesContents
    { cabalContents   :: !Text
      -- ^ The package Cabal file contents.
    , projectContents :: !Text
      -- ^ The @cabal.project@ file contents.
    }

--------------------------------------------------------------------------------
-- Fetching.

-- | Fetch the sources of a 'CabalPlan', calling @cabal get@ on each
-- package and putting it into the correspondingly named and versioned
-- subfolder of the specified directory (e.g. @pkg-name-1.2.3@).
fetchPlan :: Verbosity
          -> Cabal
          -> SymbolicPath CWD ( Dir Project )
          -> Maybe IndexState
          -> SymbolicPath Project ( Dir Fetch )  -- ^ Directory in which to put the sources.
          -> CabalPlan
          -> IO ()
fetchPlan verbosity cabal workDir mbIndexState fetchDir cabalPlan =
    for_ pkgs \ (pkgNm, pkgVer) -> do
      let nameVersion = pkgNameVersion pkgNm pkgVer
          nmVerStr = Text.unpack nameVersion
      pkgDirExists <- doesDirectoryExist ( interpretSymbolicPath workDir $ fetchDir </> mkRelativePath nmVerStr )
      if   pkgDirExists
      then normalMsg verbosity $ "NOT fetching " <> nameVersion
      else cabalFetch verbosity cabal workDir mbIndexState fetchDir nmVerStr
  where
    pkgs :: Set (PkgName, Version)
    pkgs = Set.fromList
               -- Some packages might have multiple components;
               -- we don't want to fetch the package itself multiple times.
         $ mapMaybe remotePkgNameVersion
         $ planUnits cabalPlan

    remotePkgNameVersion :: PlanUnit -> Maybe (PkgName, Version)
    remotePkgNameVersion = \case
      PU_Configured ( ConfiguredUnit { puPkgName = nm, puVersion = ver, puPkgSrc = src } )
        | Remote <- src -- only fetch remote packages
        -> Just (nm, ver)
      _ -> Nothing

-- | Call @cabal get@ to fetch a single package from Hackage.
cabalFetch :: Verbosity
           -> Cabal
           -> SymbolicPath CWD ( Dir Project )
           -> Maybe IndexState
           -> SymbolicPath Project ( Dir Fetch )
           -> String
           -> IO ()
cabalFetch verbosity cabal workDir mbIndexState root pkgNmVer = do
    normalMsg verbosity $ "Fetching " <> Text.pack pkgNmVer
    let args = globalCabalArgs cabal ++
                 [ "get"
                 , pkgNmVer
                 , cabalVerbosity verbosity ]
                 ++ [ "--index-state=" <> Text.unpack indexState
                    | IndexState indexState <- maybeToList mbIndexState ]
        procEnv =
          ProcessEnv
            { cwd = workDir </> root
            , extraEnvVars = []
            , extraPATH = []
            , logBasePath = Nothing
            }
    callProcessInIO @Fetch ( error "ghc not needed" ) cabal procEnv Nothing $
      CP { prog     = CabalProgram
         , args
         , lockFile = Nothing
        }

--------------------------------------------------------------------------------
-- Building.

-- | Build a 'CabalPlan'. This will install all the packages in the plan
-- by running their @Setup@ scripts. Libraries will be registered
-- into a local package database at @installDir/package.conf@.
buildPlan :: Verbosity
          -> SymbolicPath CWD ( Dir Project )
              -- ^ Working directory.
              -- Used to compute relative paths for local packages,
              -- and to choose a logging directory.
          -> Paths ForPrep
          -> Paths ForBuild
          -> Maybe ( AbsolutePath ( Dir Logs ) )
              -- ^ event log directory
          -> BuildStrategy
          -> Bool
             -- ^ @True@ <> resume a previously-started build,
             -- skipping over units that were already built.
             --
             -- This function will fail if this argument is @False@
             -- and one of the units has already been registered in the
             -- package database.
          -> Maybe [ UnitId ]
             -- ^ @Just units@: only build @units@ and their transitive
             -- dependencies, instead of the full build plan.
          -> ( ConfiguredUnit -> UnitArgs )
             -- ^ Extra arguments for each unit in the build plan.
          -> CabalPlan
             -- ^ Build plan to execute.
          -> IO ()
buildPlan verbosity workDir
          pathsForPrep@( Paths { buildPaths = buildPathsForPrep })
          pathsForBuild
          mbEventLogDir
          buildStrat
          resumeBuild
          mbOnlyBuildDepsOf
          userUnitArgs
          cabalPlan
  = do
    let paths@( BuildPaths { compiler, prefix, installDir } )
          = buildPaths pathsForBuild

        pkgDbDirForPrep
          = getPkgDbDirForPrep pathsForPrep

        pkgDbDirs@( PkgDbDirForBuild { finalPkgDbDir } )
          = getPkgDbDirForBuild pathsForBuild

        cabal :: Cabal
        cabal = error "cabal-install not needed for buildPlan"

    -- Check the package database exists when it should,
    -- and delete it if we are starting fresh.
    finalPkgDbExists <- doesDirectoryExist ( getAbsolutePath finalPkgDbDir )
    if | resumeBuild && not finalPkgDbExists
       -> error $ "Cannot resume build: no package database at " <> show finalPkgDbDir
       | not resumeBuild && finalPkgDbExists
       -> removeDirectoryRecursive ( getAbsolutePath finalPkgDbDir )
            `catch` \ ( _ :: IOException ) -> return ()
       | otherwise
       -> return ()

    verboseMsg verbosity $
      Text.unlines [ "Directory structure:"
                   , "    work dir: " <> Text.pack ( show workDir )
                   , "      prefix: " <> Text.pack ( show prefix )
                   , "  installDir: " <> Text.pack ( show installDir )
                   ]

    mbAlreadyBuilt <-
      if resumeBuild
      then let prepComp = compilerForPrep buildPathsForPrep
           in Just <$> getInstalledUnits verbosity prepComp buildPathsForPrep pkgDbDirForPrep fullDepMap
      else return Nothing

    let
        -- Units to build, in dependency order.
        unitsToBuild :: [ ( ConfiguredUnit, Maybe UnitId ) ]
        unitsToBuild =
          tagUnits $ sortPlan mbAlreadyBuilt mbOnlyBuildDepsOf cabalPlan

        nbUnitsToBuild :: Word
        nbUnitsToBuild = fromIntegral $ length unitsToBuild

        pkgMap :: Map ( PkgName, Version ) ConfiguredUnit
        pkgMap = Lazy.Map.fromList
          [ ((puPkgName, puVersion), cu)
          | ( cu@( ConfiguredUnit { puPkgName, puVersion } ), didSetup ) <- unitsToBuild
          , isNothing didSetup ]

        -- Initial preparation: logging, and creating the package database.
        preparation :: BuildScript prepDir ()
        preparation = do
          logMessage verbosity Verbose $
            "Creating package database at " <> show finalPkgDbDir
          createDir finalPkgDbDir

          for_ mbEventLogDir \ eventLogDir -> do
            logMessage verbosity Verbose $
              "Creating event log directory at " <> show ( getAbsolutePath eventLogDir )
            createDir eventLogDir

          logMessage verbosity Debug $ "Packages:\n" <>
            unlines
              [ "  - " <> Text.unpack (pkgNameVersion nm ver)
              | (nm, ver) <- Map.keys pkgMap ]
          logMessage verbosity Debug $ "Units:\n" <>
            unlines
              [ "  - " <> Text.unpack pkgNm <> ":" <> Text.unpack (cabalComponent compName)
              | ( ConfiguredUnit
                  { puPkgName = PkgName pkgNm
                  , puComponentName = compName }
                , _ ) <- unitsToBuild
              ]
          logMessage verbosity Normal $ "=== BUILD START ==="

        -- Setup the package for this unit.
        unitSetupScript :: ConfiguredUnit -> IO ( BuildScript Pkg ( ProcessEnv Pkg ) )
        unitSetupScript pu = do
          let pkgDirForPrep  = getPkgDir pathsForPrep  pu
              pkgDirForBuild = getPkgDir pathsForBuild pu
          setupPackage verbosity
            workDir paths pkgDbDirs pkgDirForPrep pkgDirForBuild
            fullDepMap pu

        -- Build and install this unit.
        unitBuildScript :: Args -> ConfiguredUnit -> BuildScript Pkg ( ProcessEnv Pkg )
        unitBuildScript extraConfigureArgs pu@( ConfiguredUnit { puId }) =
          let pkgDirForBuild = getPkgDir pathsForBuild pu
              mbEventLogArg = mbEventLogDir <&> \ eventLogDir ->
                let logPath = getAbsolutePath eventLogDir </> "ghc" <.> Text.unpack ( unUnitId puId ) <.> "eventlog"
                in "--ghc-option=\"-with-rtsopts=-l-ol" <> logPath <> "\""
              allConfigureArgs =
                mconcat [ extraConfigureArgs
                        , maybeToList mbEventLogArg
                        , configureArgs ( userUnitArgs pu ) ]
              allUnitArgs =
                ( userUnitArgs pu ) { configureArgs = allConfigureArgs }
          in buildUnit verbosity compiler
                 workDir paths pkgDbDirs pkgDirForBuild
                 allUnitArgs
                 fullDepMap pu

        -- Close out the build.
        finish :: BuildScript finishDir ()
        finish = do
          logMessage verbosity Normal $ "=== BUILD SUCCEEDED ==="

    case buildStrat of

      Execute runStrat -> do

        -- Initialise the "units built" counter.
        unitsBuiltCounterRef <- newIORef 0
        let unitsBuiltCounter
              = Just $
                Counter { counterRef = unitsBuiltCounterRef
                        , counterMax = nbUnitsToBuild }

            executeNoEnv :: BuildScript dir () -> IO ()
            executeNoEnv = executeBuildScriptNoProcessEnv compiler cabal unitsBuiltCounter
            execute :: BuildScript Pkg ( ProcessEnv Pkg ) -> IO ()
            execute = executeBuildScript compiler cabal unitsBuiltCounter

        case runStrat of

          Async sem -> do

            normalMsg verbosity $
              "\nBuilding and installing units asynchronously with " <> semDescription sem
            executeNoEnv preparation

            withNewAbstractSem sem \ ( AbstractSem { withAbstractSem } ) semArgs -> do

              let unitMap :: Lazy.Map UnitId ( ConfiguredUnit, Maybe UnitId )
                  unitMap =
                    Lazy.Map.fromList
                      [ ( puId, pu )
                      | pu@( ConfiguredUnit { puId }, _ ) <- unitsToBuild ]

              ( _, unitAsyncs ) <- mfix \ ~( pkgAsyncs, unitAsyncs ) -> do

                let -- Compile the Setup script of the package the unit belongs to.
                    -- (This should happen only once per package.)
                    doPkgSetupAsync :: ConfiguredUnit -> IO ()
                    doPkgSetupAsync cu@( ConfiguredUnit { puSetupDepends } ) = do

                      -- Wait for the @setup-depends@ units.
                      for_ puSetupDepends \ setupDepId ->
                        for_ ( unitAsyncs Map.!? setupDepId ) wait

                      -- Setup the package.
                      withAbstractSem do
                        setupScript <- unitSetupScript cu
                        execute setupScript

                    -- Configure, build and install the unit.
                    doUnitAsync :: ( ConfiguredUnit, Maybe UnitId ) -> IO ()
                    doUnitAsync ( pu, _didSetup ) = do

                      let nm  = Configured.puPkgName pu
                          ver = Configured.puVersion pu

                      -- Wait for the package to have been setup.
                      wait $ pkgAsyncs Map.! ( nm, ver )

                      -- Wait until we have built the units we depend on.
                      for_ ( unitDepends pu ) \ depUnitId ->
                        for_ ( unitAsyncs Map.!? depUnitId ) wait

                      -- Build the unit!
                      withAbstractSem $
                        execute $ unitBuildScript semArgs pu

                -- Kick off setting up the packages...
                finalPkgAsyncs  <- for pkgMap  ( async . doPkgSetupAsync )
                -- ... and building the units.
                finalUnitAsyncs <- for unitMap ( async . doUnitAsync )
                return ( finalPkgAsyncs, finalUnitAsyncs )
              mapM_ wait unitAsyncs
              executeNoEnv finish

          TopoSort -> do
            normalMsg verbosity "\nBuilding and installing units sequentially.\n\
                                \NB: pass -j<N> for increased parallelism."
            executeNoEnv preparation
            for_ unitsToBuild \ ( cu, didSetup ) -> do
              when ( isNothing didSetup ) $
                unitSetupScript cu >>= execute
              execute $ unitBuildScript [] cu
            executeNoEnv finish

      GenerateScript { scriptPath = fp, scriptType, useVariables } -> do
        let scriptConfig :: ScriptConfig
            scriptConfig =
              ScriptConfig
                { scriptOutput     = Script { useVariables, scriptType }
                , scriptStyle      = hostStyle
                , scriptTotal      = Just nbUnitsToBuild }
            getBuildSteps :: BuildScript dir a -> ( a, BuildSteps dir )
            getBuildSteps = buildSteps scriptConfig
            prep, end :: BuildSteps dir
            prep = snd $ getBuildSteps preparation
            end  = snd $ getBuildSteps finish
        buildScripts <-
          for unitsToBuild \ ( cu, didSetup ) -> do
            mbSetup <-
              case didSetup of
                Nothing -> Right . getBuildSteps <$> unitSetupScript cu
                Just unitDidSetup -> return $ Left unitDidSetup
            return $ ( cu, mbSetup, getBuildSteps $ unitBuildScript [] cu )
        script <-
          case scriptType of
            Shell -> do
              let emptyProcEnv :: ProcessEnv dir
                  emptyProcEnv =
                    ProcessEnv
                      { cwd          = sameDirectory
                      , extraEnvVars = []
                      , extraPATH    = []
                      , logBasePath  = Nothing
                      }
              return $
                mconcat
                  [ shellScriptHeader scriptConfig
                  , shellScriptSteps compiler cabal emptyProcEnv scriptConfig prep
                  , mconcat
                      [ shellScriptSteps compiler cabal pkgEnv scriptConfig $
                          fromRight [] ( fmap snd setup ) <> build
                      | ( _, setup, ( pkgEnv, build ) ) <- buildScripts ]
                  , shellScriptSteps compiler cabal emptyProcEnv scriptConfig end ]
            Ninja -> return $ ninja compiler hostStyle fullDepMap paths prep end buildScripts
        let what = case scriptType of { Shell -> "shell script" ; Ninja -> "ninja file"}
        normalMsg verbosity $ "\nWriting " <> what <> " to " <> Text.pack ( show fp )
        Text.writeFile ( getSymbolicPath fp ) script

  where

    -- This needs to have ALL units, as that's how we pass correct
    -- Unit IDs for dependencies.
    fullDepMap :: Map UnitId PlanUnit
    fullDepMap = Map.fromList
              [ ( planUnitUnitId pu, pu )
              | pu <- planUnits cabalPlan ]


-- | Sort the units in a 'CabalPlan' in dependency order.
sortPlan :: Maybe ( Set UnitId )
             -- ^ - @Just skip@ <=> skip these already-built units.
             --   - @Nothing@ <=> don't skip any units.
         -> Maybe [ UnitId ]
             -- ^ - @Just keep@ <=> only return units that belong
             --     to the transitive closure of @keep@.
             --   - @Nothing@ <=> return all units in the plan.
         -> CabalPlan
         -> [ ConfiguredUnit ]
sortPlan mbAlreadyBuilt mbOnlyDepsOf plan =
    onlyInteresting $ map ( fst3 . lookupVertex ) $ Graph.reverseTopSort gr
  where

    onlyInteresting :: [ ConfiguredUnit ] -> [ ConfiguredUnit ]
    onlyInteresting
      -- Fast path: don't filter out anything.
      | isNothing mbAlreadyBuilt
      , isNothing mbOnlyDepsOf
      = id
      | otherwise
      = filter isInteresting

      where
        isInteresting :: ConfiguredUnit -> Bool
        isInteresting cu@( ConfiguredUnit { puId } )
          | not $ reachable cu
          = False
          | Just alreadyBuilt <- mbAlreadyBuilt
          , puId `Set.member` alreadyBuilt
          = False
          | otherwise
          = True

        reachable :: ConfiguredUnit -> Bool
        reachable =
          case mbOnlyDepsOf of
            Nothing -> const True
            Just onlyDepsOf ->
              let reachableUnits :: Set UnitId
                  !reachableUnits
                    = Set.fromList
                    $ map ( Configured.puId . fst3 . lookupVertex )
                    $ concatMap toList
                    $ Graph.dfs gr
                    $ mapMaybe mkVertex onlyDepsOf
              in \ ( ConfiguredUnit { puId } ) -> puId `Set.member` reachableUnits

    fst3 :: ( a, b, c ) -> a
    fst3 ( a, _, _ ) = a
    ( gr, lookupVertex, mkVertex ) =
      Graph.graphFromEdges
        [ ( pu, puId, allDepends pu )
        | PU_Configured pu@( ConfiguredUnit { puId } ) <- planUnits plan ]

-- | Tag units in a build plan: the first unit we compile in each package
-- is tagged (with @'Nothing'@) as having the responsibility to build
-- the Setup executable for the package it belongs to, while other units
-- in this same package are tagged with @'Just' uid@, where @uid@ is the unit
-- which is responsible for building the Setup executable.
tagUnits :: [ ConfiguredUnit ] -> [ ( ConfiguredUnit, Maybe UnitId ) ]
tagUnits = go Map.empty
  where
    go _ [] = []
    go seenPkgs ( cu@( ConfiguredUnit { puId } ):cus)
      | puId == dummyUnitId
      = go seenPkgs cus
      | let nm  = Configured.puPkgName cu
            ver = Configured.puVersion cu
      , ( mbUnit, newPkgs ) <- Map.insertLookupWithKey (\_ a _ -> a) (nm,ver) puId seenPkgs
      = (cu, mbUnit) : go newPkgs cus

-- | Compute the set of @UnitId@s that have already been installed, to avoid
-- unnecessarily recompiling them.
--
-- This set of already-installed units is computed by querying the following:
--
--  - Library: is it already registered in the package database?
--  - Executable: is there an executable of the correct name in the binary
--    directory associated with the unit?
getInstalledUnits :: Verbosity
                  -> Compiler
                  -> BuildPaths ForPrep
                  -> PkgDbDir ForPrep
                  -> Map UnitId PlanUnit
                  -> IO ( Set UnitId )
getInstalledUnits verbosity
                  ( Compiler { ghcPkgPath } )
                  ( BuildPathsForPrep { installDir } )
                  ( PkgDbDirForPrep { finalPkgDbDir } )
                  plan = do
  pkgVerUnitIds <-
    words <$>
    Process.readProcess ( getAbsolutePath ghcPkgPath )
      [ "list"
      , ghcPkgVerbosity verbosity
      , "--show-unit-ids", "--simple-output"
      , "--package-db", getAbsolutePath finalPkgDbDir ]
        -- TODO: allow user package databases too?
      ""
  let installedLibs = map ( UnitId . Text.pack ) pkgVerUnitIds
  verboseMsg verbosity $
    "Preinstalled libraries:\n" <> Text.unlines ( map mkLine installedLibs )

  binDirContents <- listDirectory binsDir
  installedBins  <- catMaybes <$> mapM binDirMaybe binDirContents
  verboseMsg verbosity $
    "Preinstalled executables:\n" <> Text.unlines ( map mkLine installedBins )

  return $ Set.fromList installedLibs <> Set.fromList installedBins
  where

    mkLine :: UnitId -> Text
    mkLine ( UnitId uid ) = "  - " <> uid

    binsDir :: FilePath
    binsDir = getAbsolutePath installDir </> "bin"
    binDirMaybe :: FilePath -> IO ( Maybe UnitId )
    binDirMaybe binDir = do
      isDir <- doesDirectoryExist ( binsDir </> binDir )
      if not isDir
      then return Nothing
      else
        case plan Map.!? ( UnitId $ Text.pack binDir ) of
          -- Is this directory name the 'UnitId' of an executable
          -- in the build plan?
          Just ( PU_Configured cu )
            | ConfiguredUnit
              { puId
              , puComponentName =
                  ComponentName
                    { componentName = comp
                    , componentType = Exe }
              } <- cu
            -> do -- If so, does it contain the executable we expect?
                  let exePath = binsDir </> binDir </> Text.unpack comp <.> exeExtension
                  exeExists <- doesFileExist exePath
                  if exeExists
                  then return $ Just puId
                  else return Nothing
          _ -> return Nothing
