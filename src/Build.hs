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
  ( async, forConcurrently, wait )

-- bytestring
import qualified Data.ByteString.Lazy as Lazy.ByteString
  ( readFile )

-- containers
import qualified Data.Graph as Graph
  ( graphFromEdges', reverseTopSort )
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( (!?), assocs, elems, fromList, keys, lookup, mapMaybe )
import qualified Data.Map.Lazy as Lazy
  ( Map )
import qualified Data.Map.Lazy as Lazy.Map
  ( fromList, fromListWith )
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( elems, fromList, toList )

-- directory
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing
  , doesDirectoryExist )

-- filepath
import System.FilePath
  ( (</>), (<.>)
  , isAbsolute
  )

-- text
import Data.Text
  ( Text )
import qualified Data.Text    as Text
  ( all, intercalate, pack, unlines, unpack, unwords )
import qualified Data.Text.IO as Text
  ( writeFile )

-- build-env
import BuildOne
  ( buildUnit, preparePackage )
import CabalPlan
import Config
import Script
  ( BuildScript, runBuildScript, script )
import Target
  ( TargetArgs, lookupTargetArgs )
import Utils
  ( CallProcess(..), callProcessInIO, withTempDir )

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

-- | The contents of a dummy @cabal@ file with dependencies on
-- the specified units (without any constraints).
--
-- The corresponding package Id is 'dummyPackageId'.
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

    normalMsg verbosity $ "\nPreparing packages for build"
    verboseMsg verbosity $
      Text.unlines [ "Directory structure:"
                   , "      prefix: " <> Text.pack prefix
                   , "     destDir: " <> Text.pack destDir
                   , "  installDir: " <> Text.pack installDir ]
    debugMsg verbosity $ "Packages:\n" <>
      Text.unlines
        [ "  - " <> pkgNameVersion nm ver
        | (nm, ver) <- Map.keys pkgs ]

    let forPkgs :: Traversable t => t a -> (a -> IO b) -> IO (t b)
        forPkgs = case buildStrat of { Async -> forConcurrently; _ -> for }
    setups <-
      forPkgs pkgs \ ( nm, ver, pkgSrc ) ->
        preparePackage fetchDir dest nm ver pkgSrc

    let unitBuildScript :: ConfiguredUnit -> BuildScript
        unitBuildScript pu@(ConfiguredUnit { puPkgName, puVersion, puComponentName }) =
          let pkgConfigureArgs =
                lookupTargetArgs configureArgs puPkgName puComponentName
              setupHs = case Map.lookup ( puPkgName, puVersion ) setups of
                Nothing    -> error $ "buildPlan: could not find Setup.hs for "
                                   <> Text.unpack (pkgNameVersion puPkgName puVersion)
                Just setup -> setup
          in buildUnit verbosity comp fetchDir dest
               pkgConfigureArgs ghcPkgArgs
               depMap pu setupHs

    debugMsg verbosity $ "Units to build:\n" <>
      Text.unlines
        [ "  - " <> cabalComponent compName
        | ConfiguredUnit { puComponentName = compName } <- unitsToBuild
        ]

    case buildStrat of
      Async -> do
        normalMsg verbosity "Building and registering packages asynchronously"
        unitAsyncs <- mfix \ unitAsyncs ->
          let doPkgAsync :: ConfiguredUnit -> IO ()
              doPkgAsync pu = do
                  for_ (allDepends pu) \ depUnitId ->
                    for_ (unitAsyncs Map.!? depUnitId) \ cu ->
                      -- (Nothing for Preexisting packages)
                      wait cu
                  runBuildScript $ unitBuildScript pu
           in for unitMap (async . doPkgAsync)
        mapM_ wait unitAsyncs
      TopoSort -> do
        normalMsg verbosity "Building and registering packages sequentially.\n\
                            \NB: pass --async for increased parallelism."
        for_ unitsToBuild (runBuildScript . unitBuildScript)
      Script fp -> do
        normalMsg verbosity $ "Writing build script to " <> Text.pack fp
        Text.writeFile fp (script $ concatMap unitBuildScript unitsToBuild)

  where

    pkgs :: Lazy.Map (PkgName, Version) (PkgName, Version, PkgSrc)
    pkgs = Lazy.Map.fromListWith comb
      [ ( (puPkgName, puVersion), ( puPkgName, puVersion, puPkgSrc ) )
      | ConfiguredUnit { puPkgName, puVersion, puPkgSrc } <- unitsToBuild ]
      where
         comb :: (PkgName, Version, PkgSrc)
              -> (PkgName, Version, PkgSrc)
              -> (PkgName, Version, PkgSrc)
         comb ( n, v, s1 ) ( _, _, s2 ) = ( n, v, s1 <> s2 )

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
