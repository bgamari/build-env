{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , exeExtension, listDirectory, removeDirectoryRecursive
  )

-- filepath
import System.FilePath
  ( (</>), (<.>)
  , isAbsolute
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
  ( PkgDbDirs(..)
  , getPkgDbDirsForPrep, getPkgDbDirsForBuild, getPkgDir
  , setupPackage, buildUnit )
import BuildEnv.CabalPlan
import qualified BuildEnv.CabalPlan as Configured
  ( ConfiguredUnit(..) )
import BuildEnv.Config
import BuildEnv.Script
  ( BuildScript, ScriptOutput(..), ScriptConfig(..)
  , emptyBuildScript
  , executeBuildScript, script
  , createDir, logMessage
  )
import BuildEnv.Utils
  ( ProgPath(..), CallProcess(..), callProcessInIO, withTempDir
  , AbstractSem(..), noSem, newAbstractSem
  )

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
            -> CabalFilesContents
            -> IO CabalPlanBinary
computePlan delTemp verbosity comp cabal ( CabalFilesContents { cabalContents, projectContents } ) =
  withTempDir delTemp "build" \ dir -> do
    verboseMsg verbosity $ "Computing plan in build directory " <> Text.pack dir
    Text.writeFile (dir </> "cabal" <.> "project") projectContents
    Text.writeFile (dir </> dummyPackageName <.> "cabal") cabalContents
    let cabalBuildArgs =
          globalCabalArgs cabal ++
            [ "build"
            , "--dry-run"
            , "--with-compiler", ghcPath comp
            , cabalVerbosity verbosity ]
    debugMsg verbosity $
      Text.unlines $ "cabal" : map ( ("  " <>) . Text.pack ) cabalBuildArgs
    callProcessInIO Nothing $
      CP { cwd          = dir
         , prog         = AbsPath $ cabalPath cabal
         , args         = cabalBuildArgs
         , extraPATH    = []
         , extraEnvVars = []
         , sem          = noSem }

    let planPath = dir </> "dist-newstyle" </> "cache" </> "plan.json"
    CabalPlanBinary <$> Lazy.ByteString.readFile planPath

-- | The contents of a dummy @cabal.project@ file, specifying
-- package constraints, flags and allow-newer.
cabalProjectContentsFromPackages :: FilePath -> UnitSpecs -> PkgSpecs -> AllowNewer -> Text
cabalProjectContentsFromPackages workDir units pins (AllowNewer allowNewer) =
      packages
   <> allowNewers
   <> flagSpecs
   <> constraints
  where

    packages
      | null localPkgs
      = "packages: .\n\n"
      | otherwise
      = Text.intercalate ",\n          "
        ( "packages: ." : map ( Text.pack . makeAbsolute ) ( Map.elems localPkgs ) )
      <> "\n"

    makeAbsolute :: FilePath -> FilePath
    makeAbsolute fp
      | isAbsolute fp
      = fp
      | otherwise
      = workDir </> fp

    isLocal :: (PkgSrc, PkgSpec, Set ComponentName) -> Maybe FilePath
    isLocal ( Local src, _, _ ) = Just src
    isLocal _ = Nothing
    localPkgs = Map.mapMaybe isLocal units

    allPkgs = fmap ( \ ( _, spec, _ ) -> spec ) units
            `unionPkgSpecsOverriding`
              pins
      -- Constraints from the SEED file (units) should override
      -- constraints from the cabal.config file (pins).

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
          -> FilePath  -- ^ Directory in which to put the sources.
          -> CabalPlan
          -> IO ()
fetchPlan verbosity cabal fetchDir cabalPlan =
    for_ pkgs \ (pkgNm, pkgVer) -> do
      let nameVersion = pkgNameVersion pkgNm pkgVer
          nmVerStr = Text.unpack nameVersion
      pkgDirExists <- doesDirectoryExist (fetchDir </> nmVerStr)
      if   pkgDirExists
      then normalMsg verbosity $ "NOT fetching " <> nameVersion
      else cabalFetch verbosity cabal fetchDir nmVerStr
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
cabalFetch :: Verbosity -> Cabal -> FilePath -> String -> IO ()
cabalFetch verbosity cabal root pkgNmVer = do
    normalMsg verbosity $ "Fetching " <> Text.pack pkgNmVer
    let args = globalCabalArgs cabal ++
                 [ "get"
                 , pkgNmVer
                 , cabalVerbosity verbosity ]
    callProcessInIO Nothing $
      CP { cwd          = root
         , prog         = AbsPath $ cabalPath cabal
         , args
         , extraPATH    = []
         , extraEnvVars = []
         , sem          = noSem }

--------------------------------------------------------------------------------
-- Building.

-- | Build a 'CabalPlan'. This will install all the packages in the plan
-- by running their @Setup@ scripts. Libraries will be registered
-- into a local package database at @<install-dir>/package.conf@.
buildPlan :: Verbosity
          -> FilePath
              -- ^ Working directory
              -- (used only to compute relative paths for local packages).
          -> Paths ForPrep
          -> Paths ForBuild
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
          buildStrat
          resumeBuild
          mbOnlyBuildDepsOf
          userUnitArgs
          cabalPlan
  = do
    let paths@( BuildPaths { compiler, prefix, destDir, installDir } )
         = buildPaths pathsForBuild
    -- Create the temporary package database, if it doesn't already exist.
    -- We also create the final installation package database,
    -- but this happens later, in (*), as part of the build script itself.
    --
    -- See Note [Using two package databases] in BuildOne.
    let pkgDbDirsForPrep@( PkgDbDirsForPrep { tempPkgDbDir } )
          = getPkgDbDirsForPrep pathsForPrep
    tempPkgDbExists <- doesDirectoryExist tempPkgDbDir

    if
      | resumeBuild && not tempPkgDbExists
      -> error $
          "Cannot resume build: no package database at " <> tempPkgDbDir
      | not resumeBuild
      -> do when tempPkgDbExists $
              removeDirectoryRecursive tempPkgDbDir
                `catch` \ ( _ :: IOException ) -> return ()
            createDirectoryIfMissing True tempPkgDbDir
      | otherwise
      -> return ()

    pkgDbDirsForBuild@( PkgDbDirsForBuild { finalPkgDbDir } )
      <- getPkgDbDirsForBuild pathsForBuild

    verboseMsg verbosity $
      Text.unlines [ "Directory structure:"
                   , "      prefix: " <> Text.pack prefix
                   , "     destDir: " <> Text.pack destDir
                   , "  installDir: " <> Text.pack installDir ]

    mbAlreadyBuilt <-
      if resumeBuild
      then let prepComp = compilerForPrep buildPathsForPrep
           in Just <$> getInstalledUnits verbosity prepComp buildPathsForPrep pkgDbDirsForPrep fullDepMap
      else return Nothing

    let -- Units to build, in dependency order.
        unitsToBuild :: [(ConfiguredUnit, Maybe UnitId)]
        unitsToBuild
           = tagUnits $ sortPlan mbAlreadyBuilt mbOnlyBuildDepsOf cabalPlan

        nbUnitsToBuild :: Word
        nbUnitsToBuild = fromIntegral $ length unitsToBuild

        pkgMap :: Map (PkgName, Version) ConfiguredUnit
        pkgMap = Lazy.Map.fromList
          [ ((puPkgName, puVersion), cu)
          | ( cu@( ConfiguredUnit { puPkgName, puVersion } ), didSetup ) <- unitsToBuild
          , isNothing didSetup ]

        -- Initial preparation: logging, and creating the final
        -- package database.
        preparation :: BuildScript
        preparation = do
          logMessage verbosity Verbose $
            "Creating final package database at " <> finalPkgDbDir
          createDir finalPkgDbDir -- (*)
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
        unitSetupScript :: ConfiguredUnit -> IO BuildScript
        unitSetupScript pu = do
          let pkgDirForPrep  = getPkgDir workDir pathsForPrep  pu
              pkgDirForBuild = getPkgDir workDir pathsForBuild pu
          setupPackage verbosity compiler
            paths pkgDbDirsForBuild pkgDirForPrep pkgDirForBuild
            fullDepMap pu

        -- Build and install this unit.
        unitBuildScript :: ConfiguredUnit -> BuildScript
        unitBuildScript pu =
          let pkgDirForBuild = getPkgDir workDir pathsForBuild pu
          in buildUnit verbosity compiler
                paths pkgDbDirsForBuild pkgDirForBuild
                (userUnitArgs pu)
                fullDepMap pu

        -- Close out the build.
        finish :: BuildScript
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

        case runStrat of

          Async sem -> do

            normalMsg verbosity $
              "\nBuilding and installing units asynchronously with " <> semDescription sem
            executeBuildScript unitsBuiltCounter preparation

            let unitMap :: Lazy.Map UnitId (ConfiguredUnit, Maybe UnitId)
                unitMap =
                  Lazy.Map.fromList
                    [ (puId, pu)
                    | pu@( ConfiguredUnit { puId }, _ ) <- unitsToBuild ]

            AbstractSem { withAbstractSem } <- newAbstractSem sem
            (_, unitAsyncs) <- mfix \ ~(pkgAsyncs, unitAsyncs) -> do

              let -- Compile the Setup script of the package the unit belongs to.
                  -- (This should happen only once per package.)
                  doPkgSetupAsync :: ConfiguredUnit -> IO ()
                  doPkgSetupAsync cu@( ConfiguredUnit { puSetupDepends } ) = do

                    -- Wait for the @setup-depends@ units.
                    for_ puSetupDepends \ setupDepId ->
                      for_ (unitAsyncs Map.!? setupDepId) wait

                    -- Setup the package.
                    withAbstractSem do
                      setupScript <- unitSetupScript cu
                      executeBuildScript unitsBuiltCounter setupScript

                  -- Configure, build and install the unit.
                  doUnitAsync :: ( ConfiguredUnit, Maybe UnitId ) -> IO ()
                  doUnitAsync ( pu, _didSetup ) = do

                    let nm  = Configured.puPkgName pu
                        ver = Configured.puVersion pu

                    -- Wait for the package to have been setup.
                    wait $ pkgAsyncs Map.! (nm, ver)

                    -- Wait until we have built the units we depend on.
                    for_ (unitDepends pu) \ depUnitId ->
                      for_ (unitAsyncs Map.!? depUnitId) wait

                    -- Build the unit!
                    withAbstractSem $
                      executeBuildScript unitsBuiltCounter $ unitBuildScript pu

              -- Kick off setting up the packages...
              finalPkgAsyncs  <- for pkgMap  (async . doPkgSetupAsync)
              -- ... and building the units.
              finalUnitAsyncs <- for unitMap (async . doUnitAsync)
              return (finalPkgAsyncs, finalUnitAsyncs)
            mapM_ wait unitAsyncs
            executeBuildScript unitsBuiltCounter finish

          TopoSort -> do
            normalMsg verbosity "\nBuilding and installing units sequentially.\n\
                                \NB: pass -j<N> for increased parallelism."
            executeBuildScript unitsBuiltCounter preparation
            for_ unitsToBuild \ ( cu, didSetup ) -> do
              when (isNothing didSetup) $
                unitSetupScript cu >>= executeBuildScript unitsBuiltCounter
              executeBuildScript unitsBuiltCounter (unitBuildScript cu)
            executeBuildScript unitsBuiltCounter finish

      Script { scriptPath = fp, useVariables } -> do
        let scriptConfig :: ScriptConfig
            scriptConfig =
              ScriptConfig { scriptOutput = Shell { useVariables }
                           , scriptStyle  = hostStyle
                           , scriptTotal  = Just nbUnitsToBuild }

        normalMsg verbosity $ "\nWriting build scripts to " <> Text.pack fp
        buildScripts <- for unitsToBuild \ ( cu, didSetup ) -> do
          mbSetup <- if   isNothing didSetup
                     then unitSetupScript cu
                     else return emptyBuildScript
          let build = unitBuildScript cu
          return $ mbSetup <> build
        Text.writeFile fp $ script scriptConfig $
          preparation <> mconcat buildScripts <> finish

  where

    -- This needs to have ALL units, as that's how we pass correct
    -- Unit IDs for dependencies.
    fullDepMap :: Map UnitId PlanUnit
    fullDepMap = Map.fromList
              [ (planUnitUnitId pu, pu)
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
         -> [ConfiguredUnit]
sortPlan mbAlreadyBuilt mbOnlyDepsOf plan =
    onlyInteresting $ map (fst3 . lookupVertex) $ Graph.reverseTopSort gr
  where

    onlyInteresting :: [ConfiguredUnit] -> [ConfiguredUnit]
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

    fst3 :: (a,b,c) -> a
    fst3 (a,_,_) = a
    ( gr, lookupVertex, mkVertex ) =
      Graph.graphFromEdges
        [ (pu, puId, allDepends pu)
        | PU_Configured pu@( ConfiguredUnit { puId } ) <- planUnits plan ]

-- | Tag units in a build plan: the first unit we compile in each package
-- is tagged (with @'Nothing'@) as having the responsibility to build
-- the Setup executable for the package it belongs to, while other units
-- in this same package are tagged with @'Just' uid@, where @uid@ is the unit
-- which is responsible for building the Setup executable.
tagUnits :: [ConfiguredUnit] -> [(ConfiguredUnit, Maybe UnitId)]
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
                  -> PkgDbDirs ForPrep
                  -> Map UnitId PlanUnit
                  -> IO ( Set UnitId )
getInstalledUnits verbosity
                  ( Compiler { ghcPkgPath } )
                  ( BuildPathsForPrep { installDir } )
                  ( PkgDbDirsForPrep { tempPkgDbDir } )
                  plan = do
  pkgVerUnitIds <-
    words <$>
    Process.readProcess ( ghcPkgPath )
      [ "list"
      , ghcPkgVerbosity verbosity
      , "--show-unit-ids", "--simple-output"
      , "--package-db", tempPkgDbDir ]
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
    binsDir = installDir </> "bin"
    binDirMaybe :: FilePath -> IO (Maybe UnitId)
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
