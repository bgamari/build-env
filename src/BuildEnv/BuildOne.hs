{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  BuildEnv.BuildOne
-- Description :  Configure, build and install a single unit
--
-- 'setupPackage' prepares a package for building, returning instructions
-- that compile its @Setup@ script.
--
-- 'buildUnit' computes build instructions to configure, build and install
-- the unit using its @Setup@ script. If the unit is a library, the instructions
-- will also register it into a local package database using @ghc-pkg@.
module BuildEnv.BuildOne
  ( -- * Building packages
    setupPackage, buildUnit

    -- * Package directory structure helpers

    -- $twoDBs

  , PkgDir(..)
  , getPkgDir
  , PkgDbDir(..)
  , getPkgDbDirForPrep, getPkgDbDirForBuild,
  ) where

-- base
import Control.Concurrent
  ( QSem, newQSem )
import Data.Foldable
  ( for_ )
import Data.Kind
  ( Type )
import Data.Maybe
  ( maybeToList )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( lookup )

-- directory
import System.Directory

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( unpack )

-- build-env
import BuildEnv.Config
import BuildEnv.CabalPlan
import BuildEnv.Script
import BuildEnv.Path
import BuildEnv.Utils
  ( ProgPath(..), CallProcess(..)
  , abstractQSem, noSem
  )

--------------------------------------------------------------------------------
-- Setup

-- | Setup a single package.
--
-- Returns a build script which compiles the @Setup@ script.
setupPackage :: Verbosity
             -> Compiler
             -> SymbolicPath CWD ( Dir Project )
             -> BuildPaths ForBuild -- ^ Overall build directory structure.
             -> PkgDbDir   ForBuild -- ^ Package database directory (see 'getPkgDbDirForBuild').
             -> PkgDir     ForPrep  -- ^ Package directory (to find the @Setup.hs@).
             -> PkgDir     ForBuild -- ^ Package directory (to build the @Setup.hs@).
             -> Map UnitId PlanUnit -- ^ All dependencies in the build plan.
             -> ConfiguredUnit      -- ^ The unit to build.
             -> IO BuildScript
setupPackage verbosity
             ( Compiler { ghcPath } )
             workDir
             paths@( BuildPaths { installDir, logDir } )
             ( PkgDbDirForBuild { finalPkgDbDir } )
             ( PkgDir { pkgNameVer, pkgDir = prepPkgDir } )
             ( PkgDir { pkgDir = buildPkgDir } )
             plan
             unit@( ConfiguredUnit { puId, puSetupDepends, puExeDepends } )

  = do let logPath :: Maybe ( AbsolutePath File )
           logPath
             | verbosity <= Quiet
             = Nothing
             | otherwise
             = Just $ logDir </> mkRelativePath ( Text.unpack ( unUnitId puId ) )
       -- Find the appropriate Setup.hs file (creating one if necessary)
       setupHs <- findSetupHs workDir prepPkgDir
       return do
         scriptCfg <- askScriptConfig
         let isCabalDep uid = case Map.lookup uid plan of
               Nothing -> error $ "setupPackage: cannot find setup dependency " ++ show uid
               Just pu -> planUnitPkgName pu == PkgName "Cabal"
             extraCabalSetupDep = not $ any isCabalDep puSetupDepends
             setupArgs = [ quoteArg ExpandVars scriptCfg ( getSymbolicPath $ setupHs )
                         , "-o"
                         , quoteArg ExpandVars scriptCfg ( getSymbolicPath $ mkRelativePath "Setup" )
                         , "-package-db=" <> quoteArg ExpandVars scriptCfg ( getAbsolutePath finalPkgDbDir )
                         , ghcVerbosity verbosity
                         ] ++ map unitIdArg puSetupDepends
                           ++ [ "-package Cabal" | extraCabalSetupDep ]
                              -- Add an implicit dependency on the 'Cabal' library
                              -- if there isn't an explicit dependency on it.

             -- Specify location of binaries and data directories.
             --
             -- See the commentary around 'depDataDirs' in 'buildUnit'.
             binDirs = [ quoteArg ExpandVars scriptCfg $ getAbsolutePath $ installDir </> mkRelativePath ( "bin" </> Text.unpack ( unUnitId exeUnitId ) )
                       | exeUnitId <- puExeDepends ]
             setupDepDataDirs =
               dataDirs scriptCfg paths
                 [ dep_cu
                 | depUnitId <- puSetupDepends
                 , let dep = lookupDependency unit depUnitId plan
                 , dep_cu <- maybeToList $ configuredUnitMaybe dep
                 ]
         logMessage verbosity Verbose $
           "Compiling Setup.hs for " <> pkgNameVer
         callProcess @Pkg $
           CP { cwd          = workDir </> buildPkgDir
              , prog         = AbsPath ghcPath
              , args         = setupArgs
              , extraPATH    = binDirs
              , extraEnvVars = setupDepDataDirs
              , logBasePath  = logPath
              , sem          = noSem
              }

-- | Find the @Setup.hs@/@Setup.lhs@ file to use,
-- or create one using @main = defaultMain@ if none exist.
findSetupHs :: SymbolicPath CWD ( Dir Project )
            -> SymbolicPath Project ( Dir Pkg )
            -> IO ( RelativePath Pkg File )
findSetupHs workDir root =
  trySetupsOrUseDefault [ mkRelativePath "Setup.hs", mkRelativePath "Setup.lhs" ]
  where

    useDefaultSetupHs = do
        let path = root </> mkRelativePath "Setup.hs"
        writeFile ( interpretSymbolicPath workDir path ) defaultSetupHs
        return $ mkRelativePath "Setup.hs"

    try fname = do
        let path = root </> fname
        exists <- doesFileExist ( interpretSymbolicPath workDir path )
        return $ if exists then Just fname else Nothing

    defaultSetupHs = unlines
        [ "import Distribution.Simple"
        , "main = defaultMain"
        ]

    trySetupsOrUseDefault [] = useDefaultSetupHs
    trySetupsOrUseDefault ( setupPath : setups ) = do
      res <- try setupPath
      case res of
        Nothing    -> trySetupsOrUseDefault setups
        Just setup -> return setup

--------------------------------------------------------------------------------
-- Build

-- | Return build steps to to configure, build and and installing the unit,
-- including registering it in the package database if it is a library.
--
-- You can run the build script with 'executeBuildScript', or you can
-- turn it into a shell script with 'script'.
--
-- Note: executing the build script will fail if the unit has already been
-- registered in the package database.
buildUnit :: Verbosity
          -> Compiler
          -> SymbolicPath CWD ( Dir Project )
          -> BuildPaths ForBuild -- ^ Overall build directory structure.
          -> PkgDbDir   ForBuild -- ^ Package database directory (see 'getPkgDbDirForBuild').
          -> PkgDir     ForBuild -- ^ This package's directory (see 'getPkgDir').
          -> UnitArgs            -- ^ Extra arguments for this unit.
          -> Map UnitId PlanUnit -- ^ All dependencies in the build plan.
          -> ConfiguredUnit      -- ^ The unit to build.
          -> BuildScript
buildUnit verbosity
          ( Compiler { ghcPath, ghcPkgPath } )
          workDir
          paths@( BuildPaths { installDir, prefix, logDir } )
          ( PkgDbDirForBuild
            { finalPkgDbDir
            , finalPkgDbSem } )
          ( PkgDir { pkgDir, pkgNameVer } )
          ( UnitArgs { configureArgs = userConfigureArgs
                     , mbHaddockArgs = mbUserHaddockArgs
                     , registerArgs  = userGhcPkgArgs } )
          plan unit@( ConfiguredUnit { puId, puDepends, puExeDepends } )
  = let compName     = Text.unpack $ cabalComponent ( puComponentName unit )
        thisUnit'sId = Text.unpack $ unUnitId puId
        logPath :: Maybe ( AbsolutePath File )
        logPath
          | verbosity <= Quiet
          = Nothing
          | otherwise
          = Just $ logDir </> mkRelativePath thisUnit'sId
        unitPrintableName
          | verbosity >= Verbose
          = pkgNameVer <> ":" <> compName
          | otherwise
          = compName
        setupDir :: SymbolicPath CWD ( Dir Pkg )
        setupDir = workDir </> pkgDir

    in do
      scriptCfg <- askScriptConfig

      let -- Specify the data directories for all dependencies,
          -- including executable dependencies (see (**)).
          -- This is important so that e.g. 'happy' can find its datadir.
          setupEnvVars =
            dataDirs scriptCfg paths
              [ dep_cu
              | depUnitId <- unitDepends unit -- (**) depends ++ exeDepends
              , let dep = lookupDependency unit depUnitId plan
              , dep_cu <- maybeToList $ configuredUnitMaybe dep
              ]

      -- Configure
      let flagsArg = case puFlags unit of
            flags
              | flagSpecIsEmpty flags
              -> []
              | otherwise
              -> [ "--flags=" <> quoteArg ExpandVars scriptCfg ( Text.unpack $ showFlagSpec flags ) ]

          -- NB: make sure to update the readme after changing
          -- the arguments that are passed here.

          -- Non-essential arguments (can be overriden by the user).
          overridableConfigureArgs =
            [ "--libdir="     <> quoteArg EscapeVars scriptCfg ( "$prefix" </> "lib" )
            , "--libsubdir="  <> thisUnit'sId
            , "--dynlibdir="  <> quoteArg EscapeVars scriptCfg ( "$prefix" </> "dynlib"  </> pkgNameVer )
            , "--libexecdir=" <> quoteArg EscapeVars scriptCfg ( "$prefix" </> "libexec" </> pkgNameVer )
            , "--docdir="     <> quoteArg EscapeVars scriptCfg ( "$prefix" </> "doc"     </> pkgNameVer )
            , setupVerbosity verbosity
            ] ++ flagsArg -- Flags shouldn't really be overridden,
                          -- but we allow it as an expert feature.

          -- Set a different build directory for each unit,
          -- to avoid clashes when building multiple units from the same
          -- package concurrently.
          buildDir = quoteArg ExpandVars scriptCfg -- Quote to escape \ on Windows.
                   $ "temp-build" </> thisUnit'sId

          -- Arguments essential to the build; can't be overriden by the user.
          essentialConfigureArgs =
            [ "--exact-configuration"
            , "--with-compiler", quoteArg ExpandVars scriptCfg ( getAbsolutePath ghcPath )
            , "--prefix"       , quoteArg ExpandVars scriptCfg ( getAbsolutePath prefix )
            , "--cid="        <> Text.unpack ( unUnitId puId )
            , "--package-db=" <> quoteArg ExpandVars scriptCfg ( getAbsolutePath finalPkgDbDir )
            , "--datadir="    <> quoteArg EscapeVars scriptCfg ( "$prefix" </> "share" )
                -- Keep datadir in sync with the 'dataDirs' function.
            , "--datasubdir=" <> pkgNameVer
            , "--builddir="   <> buildDir
            , "--bindir="     <> quoteArg EscapeVars scriptCfg ( "$prefix" </> "bin" </> thisUnit'sId )
                -- Set a different binDir for each executable unit,
                -- so that we can know precisely which executables have been built
                -- for the purpose of resumable builds.
                -- Keep bindir in sync with 'binDirs' below.
            ] ++ map ( dependencyArg plan unit ) puDepends
              ++ [ buildTarget unit ]

          configureArgs
            =  overridableConfigureArgs
            ++ userConfigureArgs
            ++ essentialConfigureArgs

          -- Add the output binary directories to PATH, to satisfy executable
          -- dependencies during the build.
          -- Keep this in sync with --bindir in essentialConfigureArgs.
          binDirs = [ quoteArg ExpandVars scriptCfg $
                      getAbsolutePath $
                      installDir </> mkRelativePath ( "bin" </> Text.unpack ( unUnitId exeUnitId ) )
                    | exeUnitId <- puExeDepends ]

          setupExe :: ProgPath Pkg
          setupExe = RelPath $ runCwdExe scriptCfg "Setup" -- relative to pkgDir

      logMessage verbosity Verbose $
        "Configuring " <> unitPrintableName
      logMessage verbosity Debug $
        "Configure arguments:\n" <> unlines ( map ( "  " <> ) configureArgs )
      callProcess @Pkg $
        CP { cwd          = setupDir
           , prog         = setupExe
           , args         = "configure" : configureArgs
           , extraPATH    = binDirs
           , extraEnvVars = setupEnvVars
           , logBasePath  = logPath
           , sem          = noSem
           }

      -- Build
      logMessage verbosity Verbose $
        "Building " <> unitPrintableName
      callProcess @Pkg $
        CP { cwd          = setupDir
           , prog         = setupExe
           , args         = [ "build"
                            , "--builddir=" <> buildDir
                            , setupVerbosity verbosity ]
           , extraPATH    = binDirs
           , extraEnvVars = setupEnvVars
           , logBasePath  = logPath
           , sem          = noSem
           }

      -- Haddock
      for_ mbUserHaddockArgs \ userHaddockArgs -> do
        logMessage verbosity Verbose $
          "Building documentation for " <> unitPrintableName
        callProcess @Pkg $
          CP { cwd          = setupDir
             , prog         = setupExe
             , args         = [ "haddock"
                              , setupVerbosity verbosity ]
                              ++ userHaddockArgs
                              ++ [ "--builddir=" <> buildDir ]
             , extraPATH    = binDirs
             , extraEnvVars = setupEnvVars
             , logBasePath  = logPath
             , sem          = noSem
             }

       -- Copy
      logMessage verbosity Verbose $
        "Copying " <> unitPrintableName
      callProcess @Pkg $
        CP { cwd          = setupDir
           , prog         = setupExe
           , args         = [ "copy", setupVerbosity verbosity
                            , "--builddir=" <> buildDir
                            , "--target-package-db=" <> quoteArg ExpandVars scriptCfg (getAbsolutePath $ installDir </> mkRelativePath "package.conf")
                            ]
           , extraPATH    = []
           , extraEnvVars = setupEnvVars
           , logBasePath  = logPath
           , sem          = noSem
           }

       -- Register
      case cuComponentType unit of
        Lib -> do
          -- Register library.
          let pkgRegsFile = thisUnit'sId <> "-pkg-reg.conf"

          logMessage verbosity Verbose $
           mconcat [ "Registering ", unitPrintableName
                   , " in package database at:\n  "
                   , show finalPkgDbDir ]

          -- Setup register
          callProcess @Pkg $
            CP { cwd          = setupDir
               , prog         = setupExe
               , args         = [ "register", setupVerbosity verbosity
                                , "--builddir=" <> buildDir
                                , "--gen-pkg-config=" <> pkgRegsFile ]
               , extraPATH    = []
               , extraEnvVars = []
               , logBasePath  = logPath
               , sem          = noSem
               }

          -- NB: we have configured & built a single target,
          -- so there should be a single "pkg-reg.conf" file,
          -- and not a directory of registration files.

          -- ghc-pkg register
          callProcess @Pkg $
            CP { cwd          = setupDir
               , prog         = AbsPath ghcPkgPath
               , args         = [ "register"
                                , ghcPkgVerbosity verbosity
                                , "--force" ]
                                ++ userGhcPkgArgs
                                ++ [ "--package-db", quoteArg ExpandVars scriptCfg ( getAbsolutePath finalPkgDbDir )
                                   , pkgRegsFile ]
               , extraPATH    = []
               , extraEnvVars = []
               , logBasePath  = logPath
               , sem          = abstractQSem finalPkgDbSem
                 -- Take a lock to avoid contention on the package database
                 -- when building units concurrently.
               }

        _notALib -> return ()

      logMessage verbosity Normal $ "Finished building " <> unitPrintableName
      reportProgress verbosity

-- | The argument @-package-id PKG_ID@.
unitIdArg :: UnitId -> String
unitIdArg (UnitId unitId) = "-package-id " <> Text.unpack unitId

-- | The target to configure and build.
buildTarget :: ConfiguredUnit -> String
buildTarget ( ConfiguredUnit { puComponentName = comp } )
  = Text.unpack $ cabalComponent comp

-- | The argument @--dependency=PKG:COMP=UNIT_ID@.
--
-- Used to specify the 'UnitId' of a dependency to the configure script.
-- This allows us to perform a build using the specific dependencies we have
-- available, ignoring any bounds in the cabal file.
dependencyArg :: Map UnitId PlanUnit -> ConfiguredUnit -> UnitId -> String
dependencyArg fullPlan unitWeAreBuilding depUnitId
  = "--dependency=" <> Text.unpack ( mkDependency pu )
  where
    pu :: PlanUnit
    pu = lookupDependency unitWeAreBuilding depUnitId fullPlan

    mkDependency :: PlanUnit -> Text
    mkDependency ( PU_Preexisting ( PreexistingUnit { puPkgName = PkgName nm } ) )
      = nm <> "=" <> unUnitId depUnitId
    mkDependency ( PU_Configured ( ConfiguredUnit { puPkgName = PkgName pkg
                                                  , puComponentName = comp } ) )
      = pkg <> ":" <> componentName comp <> "=" <> unUnitId depUnitId

-- | Look up a dependency in the full build plan.
--
-- Throws an error if the dependency can't be found.
lookupDependency :: ConfiguredUnit      -- ^ the unit which has the dependency
                                        -- (for error messages only)
                 -> UnitId              -- ^ dependency to look up
                 -> Map UnitId PlanUnit -- ^ build plan
                 -> PlanUnit
lookupDependency ( ConfiguredUnit { puPkgName = pkgWeAreBuilding } ) depUnitId plan
  | Just pu <- Map.lookup depUnitId plan
  = pu
  | otherwise
  = error $ "buildUnit: can't find dependency in build plan\n\
            \package: " <> show pkgWeAreBuilding <> "\n\
            \dependency: " <> show depUnitId

--------------------------------------------------------------------------------
-- Directory structure computation helpers

-- | The package database directory.
type PkgDbDir :: PathUsability -> Type
data family PkgDbDir use

data instance PkgDbDir ForPrep
  = PkgDbDirForPrep
    { finalPkgDbDir  :: !( AbsolutePath ( Dir PkgDb ) )
        -- ^ Installation package database directory.
    }
data instance PkgDbDir ForBuild
  = PkgDbDirForBuild
    { finalPkgDbDir :: !( AbsolutePath ( Dir PkgDb ) )
        -- ^ Installation package database directory.
    , finalPkgDbSem :: !QSem
        -- ^ Semaphore controlling access to the installation
        -- package database.
    }

-- | Compute the paths of the package database directory we are going
-- to use.
getPkgDbDirForPrep :: Paths ForPrep -> PkgDbDir ForPrep
getPkgDbDirForPrep ( Paths { buildPaths = BuildPathsForPrep { installDir } } ) =
    PkgDbDirForPrep { finalPkgDbDir = installDir </> mkRelativePath "package.conf" }

-- | Compute the paths of the package database directory we are going
-- to use, and create some semaphores to control access to it
-- in order to avoid contention.
getPkgDbDirForBuild :: Paths ForBuild -> IO (PkgDbDir ForBuild)
getPkgDbDirForBuild ( Paths { buildPaths = BuildPaths { installDir } } ) = do
  finalPkgDbSem <- newQSem 1
  return $
    PkgDbDirForBuild { finalPkgDbDir, finalPkgDbSem }
    where
      finalPkgDbDir = installDir </> mkRelativePath "package.conf"

-- | The package @name-version@ string and its directory.
type PkgDir :: PathUsability -> Type
data PkgDir use
  = PkgDir
    { pkgNameVer :: !String
        -- ^ Package @name-version@ string.
    , pkgDir     :: !( SymbolicPath Project ( Dir Pkg ) )
        -- ^ Package directory.
    }
type role PkgDir representational
  -- Don't allow accidentally passing a @PkgDir ForPrep@ where one expects
  -- a @PkgDir ForBuild@.

-- | Compute the package directory location.
getPkgDir :: Paths use
              -- ^ Overall directory structure to base the computation off.
          -> ConfiguredUnit
              -- ^ Any unit from the package in question.
          -> PkgDir use
getPkgDir ( Paths { fetchDir } )
          ( ConfiguredUnit { puPkgName, puVersion, puPkgSrc } )
  = PkgDir { pkgNameVer, pkgDir }
    where
      pkgNameVer = Text.unpack $ pkgNameVersion puPkgName puVersion
      pkgDir
        | Local dir <- puPkgSrc
        = dir
        | otherwise
        = fetchDir </> mkRelativePath pkgNameVer

-- | Command to run an executable located the current working directory.
runCwdExe :: ScriptConfig -> String -> SymbolicPath from File
runCwdExe ( ScriptConfig { scriptOutput, scriptStyle } )
  = mkSymbolicPath . pre . ext
  where
    pre
      | Run <- scriptOutput
      = id
      | otherwise
      = ( "./" <> )
    ext
      | WinStyle <- scriptStyle
      = ( <.> "exe" )
      | otherwise
      = id

-- | An environment containing the data directory paths for the given units.
dataDirs :: ScriptConfig
         -> BuildPaths ForBuild
         -> [ ConfiguredUnit ]
         -> [ ( String, FilePath ) ]
dataDirs scriptCfg ( BuildPaths { installDir } ) units =
    [ ( mangledPkgName puPkgName <> "_datadir"
      , quoteArg ExpandVars scriptCfg $
          getAbsolutePath $ installDir </> mkRelativePath ( "share" </> Text.unpack ( pkgNameVersion puPkgName puVersion ) ) )
        -- Keep this in sync with --datadir in essentialConfigureArgs.
    | ConfiguredUnit { puPkgName, puVersion } <- units ]
