{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RoleAnnotations #-}
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
  , PkgDbDirs(..)
  , getPkgDbDirsForPrep, getPkgDbDirsForBuild,
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

-- filepath
import System.FilePath
  ( (</>), (<.>)
  , makeRelative
  )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( unpack )

-- build-env
import BuildEnv.Config
import BuildEnv.CabalPlan
import BuildEnv.Script
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
             -> BuildPaths ForBuild -- ^ Overall build directory structure.
             -> PkgDbDirs  ForBuild -- ^ Package database directories (see 'getPkgDbDirs').
             -> PkgDir     ForPrep  -- ^ Package directory (to find the @Setup.hs@).
             -> PkgDir     ForBuild -- ^ Package directory (to build the @Setup.hs@).
             -> Map UnitId PlanUnit -- ^ All dependencies in the build plan.
             -> ConfiguredUnit      -- ^ The unit to build.
             -> IO BuildScript
setupPackage verbosity
             ( Compiler { ghcPath } )
             paths@( BuildPaths { installDir, logDir } )
             ( PkgDbDirsForBuild { tempPkgDbDir } )
             ( PkgDir { pkgNameVer, pkgDir = prepPkgDir } )
             ( PkgDir { pkgDir = buildPkgDir } )
             plan
             unit@( ConfiguredUnit { puId, puSetupDepends, puExeDepends } )

  = do let logPath
             | verbosity <= Quiet
             = Nothing
             | otherwise
             = Just $ logDir </> Text.unpack ( unUnitId puId )
       -- Find the appropriate Setup.hs file (creating one if necessary)
       setupHs <- findSetupHs prepPkgDir
       return do
         scriptCfg <- askScriptConfig
         let isCabalDep uid = case Map.lookup uid plan of
               Nothing -> error $ "setupPackage: cannot find setup dependency " ++ show uid
               Just pu -> planUnitPkgName pu == PkgName "Cabal"
             extraCabalSetupDep = not $ any isCabalDep puSetupDepends
             setupArgs = [ quoteArg ExpandVars scriptCfg ( buildPkgDir </> setupHs )
                         , "-o"
                         , quoteArg ExpandVars scriptCfg ( buildPkgDir </> "Setup" )
                         , "-package-db=" <> quoteArg ExpandVars scriptCfg tempPkgDbDir
                         , ghcVerbosity verbosity
                         ] ++ map unitIdArg puSetupDepends
                           ++ [ "-package Cabal" | extraCabalSetupDep ]
                              -- Add an implicit dependency on the 'Cabal' library
                              -- if there isn't an explicit dependency on it.

             -- Specify location of binaries and data directories.
             --
             -- See the commentary around 'depDataDirs' in 'buildUnit'.
             binDirs = [ quoteArg ExpandVars scriptCfg $ installDir </> "bin" </> Text.unpack ( unUnitId exeUnitId )
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
         callProcess $
           CP { cwd          = "."
              , prog         = AbsPath ghcPath
              , args         = setupArgs
              , extraPATH    = binDirs
              , extraEnvVars = setupDepDataDirs
              , logBasePath  = logPath
              , sem          = noSem
              }

-- | Find the @Setup.hs@/@Setup.lhs@ file to use,
-- or create one using @main = defaultMain@ if none exist.
findSetupHs :: FilePath -> IO FilePath
findSetupHs root = trySetupsOrUseDefault [ "Setup.hs", "Setup.lhs" ]
  where
    useDefaultSetupHs = do
        let path = root </> "Setup.hs"
        writeFile path defaultSetupHs
        return "Setup.hs"

    try fname = do
        let path = root </> fname
        exists <- doesFileExist path
        return $ if exists then Just fname else Nothing

    defaultSetupHs = unlines
        [ "import Distribution.Simple"
        , "main = defaultMain"
        ]

    trySetupsOrUseDefault [] = useDefaultSetupHs
    trySetupsOrUseDefault (setupPath:setups) = do
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
          -> BuildPaths ForBuild -- ^ Overall build directory structure.
          -> PkgDbDirs  ForBuild -- ^ Package database directories (see 'getPkgDbDirsForBuild').
          -> PkgDir     ForBuild -- ^ This package's directory (see 'getPkgDir').
          -> UnitArgs            -- ^ Extra arguments for this unit.
          -> Map UnitId PlanUnit -- ^ All dependencies in the build plan.
          -> ConfiguredUnit      -- ^ The unit to build.
          -> BuildScript
buildUnit verbosity
          ( Compiler { ghcPath, ghcPkgPath } )
          paths@( BuildPaths { installDir, prefix, logDir } )
          ( PkgDbDirsForBuild
            { tempPkgDbDir
            , finalPkgDbDir
            , tempPkgDbSem
            , finalPkgDbSem } )
          ( PkgDir { pkgDir, pkgNameVer } )
          ( UnitArgs { configureArgs = userConfigureArgs
                     , mbHaddockArgs = mbUserHaddockArgs
                     , registerArgs  = userGhcPkgArgs } )
          plan unit@( ConfiguredUnit { puId, puDepends, puExeDepends } )
  = let compName = Text.unpack $ cabalComponent ( puComponentName unit )
        thisUnit'sId = Text.unpack ( unUnitId puId )
        logPath
          | verbosity <= Quiet
          = Nothing
          | otherwise
          = Just $ logDir </> thisUnit'sId
        unitPrintableName
          | verbosity >= Verbose
          = pkgNameVer <> ":" <> compName
          | otherwise
          = compName

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
              ++ [ ("prefix", quoteArg ExpandVars scriptCfg prefix) ]

      -- Configure
      let flagsArg = case puFlags unit of
            flags
              | flagSpecIsEmpty flags
              -> []
              | otherwise
              -> [ "--flags=" <> quoteArg ExpandVars scriptCfg ( Text.unpack (showFlagSpec flags) ) ]

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
            , "--with-compiler", quoteArg ExpandVars scriptCfg ghcPath
            , "--prefix"       , quoteArg EscapeVars scriptCfg "${pkgroot}"
            , "--cid="        <> Text.unpack ( unUnitId puId )
            , "--package-db=" <> quoteArg ExpandVars scriptCfg tempPkgDbDir
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
                      installDir </> "bin" </> Text.unpack ( unUnitId exeUnitId )
                    | exeUnitId <- puExeDepends ]

          setupExe = RelPath $ runCwdExe scriptCfg "Setup" -- relative to pkgDir

      logMessage verbosity Verbose $
        "Configuring " <> unitPrintableName
      logMessage verbosity Debug $
        "Configure arguments:\n" <> unlines (map ("  " <>) configureArgs)
      callProcess $
        CP { cwd          = pkgDir
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
      callProcess $
        CP { cwd          = pkgDir
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
        callProcess $
          CP { cwd          = pkgDir
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
      callProcess $
        CP { cwd          = pkgDir
           , prog         = setupExe
           , args         = [ "copy", setupVerbosity verbosity
                            , "--builddir=" <> buildDir
                            , "--target-package-db=" <> quoteArg ExpandVars scriptCfg (installDir </> "package.conf")
                            ]
           , extraPATH    = []
           , extraEnvVars = setupEnvVars
           , logBasePath  = logPath
           , sem          = noSem
           }

       -- Register
      case cuComponentType unit of
        Lib -> do
          -- Register library (in both the local and final package databases)
          -- See Note [Using two package databases].
          let pkgRegsFile = thisUnit'sId <> "-pkg-reg.conf"
              dirs = [ ( tempPkgDbDir,  tempPkgDbSem, "temporary", ["--inplace"], [])
                     , (finalPkgDbDir, finalPkgDbSem, "final"    , [], "--force" : userGhcPkgArgs) ]

          for_ dirs \ (pkgDbDir, pkgDbSem, desc, extraSetupArgs, extraPkgArgs) -> do

            logMessage verbosity Verbose $
             mconcat [ "Registering ", unitPrintableName, " in "
                     , desc, " package database at:\n  "
                     , pkgDbDir ]

            -- Setup register
            callProcess $
              CP { cwd          = pkgDir
                 , prog         = setupExe
                 , args         = [ "register", setupVerbosity verbosity ]
                                  ++ extraSetupArgs
                                  ++ [ "--builddir=" <> buildDir
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
            callProcess $
              CP { cwd          = pkgDir
                 , prog         = AbsPath ghcPkgPath
                 , args         = [ "register"
                                  , ghcPkgVerbosity verbosity ]
                                  ++ extraPkgArgs
                                  ++ [ "--package-db", quoteArg ExpandVars scriptCfg pkgDbDir
                                     , pkgRegsFile ]
                 , extraPATH    = []
                 , extraEnvVars = []
                 , logBasePath  = logPath
                 , sem          = abstractQSem pkgDbSem
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

{- $twoDBs
__Note [Using two package databases]__

We need __two__ distinct package databases: we might want to perform the build
in a temporary location, before everything gets placed into its final
destination. The final package database might use a specific, baked-in
installation prefix (in the sense of @Setup configure --prefix pfx@). As a
result, this package database won't be immediately usable, as we won't have
copied over the build products yet.

In order to be able to build packages in a temporary directory, we create a
temporary package database that is used for the build, making use of it
with @Setup register --inplace@.
We also register the packages into the final package database using
@ghc-pkg --force@: otherwise, @ghc-pkg@ would error because the relevant files
haven't been copied over yet.
-}

-- | The package database directories.
--
-- See Note [Using two package databases].
type PkgDbDirs :: PathUsability -> Type
data family PkgDbDirs use

data instance PkgDbDirs ForPrep
  = PkgDbDirsForPrep
    { tempPkgDbDir  :: !FilePath
        -- ^ Local package database directory.
    }
data instance PkgDbDirs ForBuild
  = PkgDbDirsForBuild
    { tempPkgDbDir  :: !FilePath
        -- ^ Local package database directory.
    , tempPkgDbSem  :: !QSem
        -- ^ Semaphore controlling access to the temporary
        -- package database.
    , finalPkgDbDir :: !FilePath
        -- ^ Installation package database directory.
    , finalPkgDbSem :: !QSem
        -- ^ Semaphore controlling access to the installation
        -- package database.
    }

-- | Compute the paths of the package database directories we are going
-- to use.
--
-- See Note [Using two package databases].
getPkgDbDirsForPrep :: Paths ForPrep -> PkgDbDirs ForPrep
getPkgDbDirsForPrep ( Paths { fetchDir } ) =
    PkgDbDirsForPrep { tempPkgDbDir = fetchDir </> "package.conf" }

-- | Compute the paths of the package database directories we are going
-- to use, and create some semaphores to control access to them
-- in order to avoid contention.
--
-- See Note [Using two package databases].
getPkgDbDirsForBuild :: Paths ForBuild -> IO (PkgDbDirs ForBuild)
getPkgDbDirsForBuild ( Paths { fetchDir, buildPaths = BuildPaths { installDir } } ) = do
  tempPkgDbSem  <- newQSem 1
  finalPkgDbSem <- newQSem 1
  return $
    PkgDbDirsForBuild { tempPkgDbDir, finalPkgDbDir, tempPkgDbSem, finalPkgDbSem }
    where
      tempPkgDbDir  = fetchDir   </> "package.conf"
      finalPkgDbDir = installDir </> "package.conf"

-- | The package @name-version@ string and its directory.
type PkgDir :: PathUsability -> Type
data PkgDir use
  = PkgDir
    { pkgNameVer :: !String
        -- ^ Package @name-version@ string.
    , pkgDir     :: !FilePath
        -- ^ Package directory.
    }
type role PkgDir representational
  -- Don't allow accidentally passing a @PkgDir ForPrep@ where one expects
  -- a @PkgDir ForBuild@.

-- | Compute the package directory location.
getPkgDir :: FilePath
              -- ^ Working directory
              -- (used only to relativise paths to local packages).
          -> Paths use
              -- ^ Overall directory structure to base the computation off.
          -> ConfiguredUnit
              -- ^ Any unit from the package in question.
          -> PkgDir use
getPkgDir workDir ( Paths { fetchDir } )
          ( ConfiguredUnit { puPkgName, puVersion, puPkgSrc } )
  = PkgDir { pkgNameVer, pkgDir }
    where
      pkgNameVer = Text.unpack $ pkgNameVersion puPkgName puVersion
      pkgDir
        | Local dir <- puPkgSrc
        = makeRelative workDir dir
          -- Give local packages paths relative to the working directory,
          -- to enable relocatable build scripts.
        | otherwise
        = fetchDir </> pkgNameVer

-- | Command to run an executable located the current working directory.
runCwdExe :: ScriptConfig -> FilePath -> FilePath
runCwdExe ( ScriptConfig { scriptOutput, scriptStyle } )
  = pre . ext
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
          installDir </> "share" </> Text.unpack (pkgNameVersion puPkgName puVersion) )
        -- Keep this in sync with --datadir in essentialConfigureArgs.
    | ConfiguredUnit { puPkgName, puVersion } <- units ]
