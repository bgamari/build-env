{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , getPkgDbDirsCan, getPkgDbDirsOut
  ) where

-- base
import Control.Concurrent
  ( QSem, newQSem )
import Data.Foldable
  ( for_ )
import Data.Kind
  ( Type )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( lookup )

-- directory
import System.Directory

-- filepath
import System.FilePath
  ( (</>), (<.>) )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( unpack )

-- build-env
import BuildEnv.Config
import BuildEnv.CabalPlan
import qualified BuildEnv.CabalPlan as Configured
  ( ConfiguredUnit(..) )
import BuildEnv.Script
import BuildEnv.Utils
  ( CallProcess(..)
  , withQSem, noSem
  )

--------------------------------------------------------------------------------
-- Setup

-- | Setup a single package.
--
-- Returns a build script which compiles the @Setup@ script.
setupPackage :: Verbosity
             -> Compiler
             -> PkgDbDirs ForOutput
             -> PkgDir    Canonicalised
             -> PkgDir    ForOutput
             -> [UnitId] -- ^ @UnitId@s of @setup-depends@
             -> IO BuildScript
setupPackage verbosity
             ( Compiler { ghcPath } )
             ( PkgDbDirsOut { tempPkgDbDir } )
             ( PkgDir { pkgNameVer, pkgDir = pkgDirCan } )
             ( PkgDir { pkgDir = pkgDirOut } )
             setupDepIds

  = do -- Find the appropriate Setup.hs file (creating one if necessary)
       setupHs <- findSetupHs pkgDirCan
       return do
         scriptCfg <- askScriptConfig
         let setupArgs = [ quoteArg scriptCfg (pkgDirOut </> setupHs)
                         , "-o"
                         , quoteArg scriptCfg (pkgDirOut </> "Setup")
                         , "-package-db=" ++ quoteArg scriptCfg tempPkgDbDir
                         , ghcVerbosity verbosity
                         ] ++ map unitIdArg setupDepIds
         logMessage verbosity Verbose $
           "Compiling Setup.hs for " <> pkgNameVer
         callProcess $
           CP { cwd          = "."
              , prog         = ghcPath
              , args         = setupArgs
              , extraPATH    = []
              , extraEnvVars = []
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
-- You can run the build script with 'runBuildScript', or you can
-- turn it into a shell script with 'script'.
--
-- Note: executing the build script will fail if the unit has already been
-- registered in the package database.
buildUnit :: Verbosity
          -> Compiler
          -> PkgDbDirs ForOutput -- ^ package database directories (see 'getPkgDbDirs')
          -> PkgDir    ForOutput -- ^ package directory (see 'getPkgDir')
          -> Dirs ForOutput
                      -- ^ directory structure
          -> UnitArgs -- ^ extra arguments for this unit
          -> Map UnitId PlanUnit -- ^ all dependencies in the build plan
          -> ConfiguredUnit -- ^ the unit to build
          -> BuildScript
buildUnit verbosity
          ( Compiler { ghcPath, ghcPkgPath } )
          ( PkgDbDirsOut
            { tempPkgDbDir
            , finalPkgDbDir
            , tempPkgDbSem
            , finalPkgDbSem } )
          ( PkgDir { pkgDir
                   , pkgNameVer } )
          ( Dirs { installDir, prefix, destDir } )
          ( UnitArgs { configureArgs = userConfigureArgs
                     , mbHaddockArgs = mbUserHaddockArgs
                     , registerArgs  = userGhcPkgArgs } )
          plan unit
  = let compName = Text.unpack $ cabalComponent ( puComponentName unit )
        thisUnitId = Text.unpack (unUnitId $ Configured.puId unit)
        unitPrintableName
          | verbosity >= Verbose
          = pkgNameVer <> ":" <> compName
          | otherwise
          = compName

    in do
      scriptCfg <- askScriptConfig

      -- Configure
      let flagsArg = case puFlags unit of
            flags
              | flagSpecIsEmpty flags
              -> []
              | otherwise
              -> [ "--flags=" ++ quoteArg scriptCfg ( Text.unpack (showFlagSpec flags) ) ]
          buildDir = quoteArg scriptCfg $ pkgDir </> "dist" </> thisUnitId
          configureArgs = [ "--with-compiler", quoteArg scriptCfg ghcPath
                          , "--prefix", quoteArg scriptCfg prefix
                          , "--cid=" ++ Text.unpack (unUnitId $ Configured.puId unit)
                          , "--package-db=" ++ quoteArg scriptCfg tempPkgDbDir
                          , "--exact-configuration"
                          , "--datasubdir=" ++ pkgNameVer
                          , "--builddir=" ++ buildDir
                          , setupVerbosity verbosity
                          ] ++ flagsArg
                            ++ userConfigureArgs
                            ++ map ( dependencyArg plan unit )
                                ( Configured.puDepends unit )
                          ++ [ buildTarget unit ]
          setupExe = runCwdExe scriptCfg "Setup"
      logMessage verbosity Verbose $
        "Configuring " <> unitPrintableName
      logMessage verbosity Debug $
        "Configure arguments:\n" <> unlines (map ("  " <>) configureArgs)
      callProcess $
        CP { cwd  = pkgDir
           , prog = setupExe
           , args = "configure" : configureArgs

             -- Add the output binary directory to PATH, to satisfy executable
             -- dependencies during the build.
           , extraPATH = [ quoteArg scriptCfg $ installDir </> "bin" ]

              -- Specify the data directories for all dependencies.
           , extraEnvVars =
               [ ( mangledPkgName depName <> "_datadir"
                 , quoteArg scriptCfg $
                   installDir </> Text.unpack (pkgNameVersion depName depVer) )
               | depUnitId <- Configured.puDepends unit
               , let dep     = lookupDependency unit depUnitId plan
                     depName = planUnitPkgName dep
                     depVer  = planUnitVersion dep
              ]
           , sem = noSem
           }

      -- Build
      logMessage verbosity Verbose $
        "Building " <> unitPrintableName
      callProcess $
        CP { cwd          = pkgDir
           , prog         = setupExe
           , args         = [ "build"
                            , "--builddir=" ++ buildDir
                            , setupVerbosity verbosity]
           , extraPATH    = []
           , extraEnvVars = []
           , sem          = noSem
           }

      -- Haddock
      case mbUserHaddockArgs of
        Nothing -> return ()
        Just userHaddockArgs -> do
          logMessage verbosity Verbose $
            "Building documentation for " <> unitPrintableName
          callProcess $
            CP { cwd          = pkgDir
               , prog         = setupExe
               , args         = [ "haddock"
                                , "--builddir=" ++ buildDir
                                , setupVerbosity verbosity ]
                                  ++ userHaddockArgs
               , extraPATH    = []
               , extraEnvVars = []
               , sem          = noSem
               }

       -- Copy
      logMessage verbosity Verbose $
        "Copying " <> unitPrintableName
      callProcess $
        CP { cwd          = pkgDir
           , prog         = setupExe
           , args         = [ "copy", setupVerbosity verbosity
                            , "--builddir=" ++ buildDir
                            , "--destdir", quoteArg scriptCfg destDir ]
           , extraPATH    = []
           , extraEnvVars = []
           , sem          = noSem
           }

       -- Register
      case cuComponentType unit of
        Lib -> do
          -- Register library (in both the local and final package databases)
          -- See Note [Using two package databases].
          let pkgRegsFile = quoteArg scriptCfg
                          $ pkgDir </> ( thisUnitId <> "-pkg-reg.conf" )
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
                 , args         = [ "register", setupVerbosity verbosity
                                  , "--builddir=" ++ buildDir
                                  , "--gen-pkg-config=" ++ pkgRegsFile
                                  ] ++ extraSetupArgs
                 , extraPATH    = []
                 , extraEnvVars = []
                 , sem          = noSem
                 }

            -- NB: we have configured & built a single target,
            -- so there should be a single "pkg-reg.conf" file,
            -- and not a directory of registration files.

            -- ghc-pkg register
            callProcess $
              CP { cwd          = "."
                 , prog         = ghcPkgPath
                 , args         = [ "register"
                                  , ghcPkgVerbosity verbosity
                                  , "--package-db", quoteArg scriptCfg pkgDbDir
                                  , pkgRegsFile ]
                                  ++ extraPkgArgs
                 , extraPATH    = []
                 , extraEnvVars = []
                 , sem          = withQSem pkgDbSem
                   -- Take a lock to avoid contention on the package database
                   -- when building units concurrently.
                 }

        _notALib -> return ()

      logMessage verbosity Normal $ "Installed " <> unitPrintableName

-- | The argument @-package-id PKG_ID@.
unitIdArg :: UnitId -> String
unitIdArg (UnitId unitId) = "-package-id " ++ Text.unpack unitId

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
  = "--dependency=" ++ Text.unpack (mkDependency pu)
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
lookupDependency unitWeAreBuilding depUnitId plan
  | Just pu <- Map.lookup depUnitId plan
  = pu
  | otherwise
  = error $ "buildUnit: can't find dependency in build plan\n\
            \unit: " ++ show (Configured.puPkgName unitWeAreBuilding) ++ "\n\
            \dependency: " ++ show depUnitId

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
type PkgDbDirs :: PathType -> Type
data family PkgDbDirs pathTy

data instance PkgDbDirs Canonicalised
  = PkgDbDirsCan
    { tempPkgDbDir  :: StagedPath Canonicalised
        -- ^ Local package database directory
    , finalPkgDbDir :: StagedPath Canonicalised
        -- ^ Installation package database directory
    }
data instance PkgDbDirs ForOutput
  = PkgDbDirsOut
    { tempPkgDbDir  :: StagedPath ForOutput
        -- ^ Local package database directory
    , tempPkgDbSem  :: QSem
        -- ^ Semaphore controlling access to the temporary
        -- package database.
    , finalPkgDbDir :: StagedPath ForOutput
        -- ^ Installation package database directory
    , finalPkgDbSem :: QSem
        -- ^ Semaphore controlling access to the installation
        -- package database.
    }

-- | Compute the paths of the package database directories we are going
-- to use.
--
-- See Note [Using two package databases].
getPkgDbDirsCan :: Dirs Canonicalised -> PkgDbDirs Canonicalised
getPkgDbDirsCan ( Dirs { fetchDir, installDir } ) =
    PkgDbDirsCan { tempPkgDbDir, finalPkgDbDir }
    where
      tempPkgDbDir  = fetchDir   </> "package.conf"
      finalPkgDbDir = installDir </> "package.conf"

-- | Compute the paths of the package database directories we are going
-- to use, and create some semaphores to control access to them
-- in order to avoid contention.
--
-- See Note [Using two package databases].
getPkgDbDirsOut :: Dirs ForOutput -> IO (PkgDbDirs ForOutput)
getPkgDbDirsOut ( Dirs { fetchDir, installDir } ) = do
  tempPkgDbSem  <- newQSem 1
  finalPkgDbSem <- newQSem 1
  return $
    PkgDbDirsOut { tempPkgDbDir, finalPkgDbDir, tempPkgDbSem, finalPkgDbSem }
    where
      tempPkgDbDir  = fetchDir   </> "package.conf"
      finalPkgDbDir = installDir </> "package.conf"

-- | The package @name-version@ string and its directory.
type PkgDir :: PathType -> Type
data PkgDir pathTy
  = PkgDir
    { pkgNameVer    :: String
        -- ^ Package @name-version@ string
    , pkgDir        :: StagedPath pathTy
        -- ^ Package directory
    }

-- | Compute some directory paths relevant to installation and registration
-- of a package.
getPkgDir :: (StagedPath pathTy ~ FilePath)
          => Dirs pathTy
          -> ConfiguredUnit -- ^ any unit from the package in question
          -> PkgDir pathTy
getPkgDir ( Dirs { fetchDir } )
          ( ConfiguredUnit { puPkgName, puVersion, puPkgSrc } )
  = PkgDir { pkgNameVer, pkgDir }
    where
      pkgNameVer = Text.unpack $ pkgNameVersion puPkgName puVersion
      pkgDir
        | Local dir <- puPkgSrc
        = dir
        | otherwise
        = fetchDir </> pkgNameVer

-- | Command to run an executable located the current working directory.
runCwdExe :: ScriptConfig -> FilePath -> FilePath
runCwdExe ( ScriptConfig { scriptOutput, scriptStyle } )
  = pre . ext
  where
    pre
      | Run      <- scriptOutput
      , WinStyle <- scriptStyle
      = id
      | otherwise
      = ( "./" <> )
    ext
      | WinStyle <- scriptStyle
      = ( <.> "exe" )
      | otherwise
      = id
