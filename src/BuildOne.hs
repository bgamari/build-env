{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  BuildOne
-- Description :  Configure, build and install a single unit
--
-- 'buildUnit' prepares the package and returns build instructions
-- to configure, build and install the unit using the @Setup.hs@ script,
-- registering it into a local package database using @ghc-pkg@.
module BuildOne
  ( -- * Building packages
    setupPackage, buildUnit

    -- * Package directory structure helpers
  , PkgDir(..), getPkgDir
  , PkgDbDirs(..), getPkgDbDirs
  ) where

-- base
import Data.Foldable
  ( for_ )

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

-- transformers
import Control.Monad.Trans.Writer.CPS
   ( execWriter )

-- build-env
import Config
import Utils ( CallProcess(..), exe )
import CabalPlan
import qualified CabalPlan as Configured
  ( ConfiguredUnit(..) )
import Script

--------------------------------------------------------------------------------
-- Setup

-- | Setup a single package.
--
-- Returns a build script which compiles the @Setup@ script.
setupPackage :: Verbosity
             -> Compiler
             -> PkgDbDirs
             -> PkgDir
             -> [UnitId] -- ^ @UnitId@s of @setup-depends@
             -> IO BuildScript
setupPackage verbosity ( Compiler { ghcPath } )
             ( PkgDbDirs { tempPkgDbDir } )
             ( PkgDir { pkgNameVer, pkgDir } )
             setupDepIds
  = do { -- Find the appropriate Setup.hs file (creating one if necessary)
       ; setupHs <- findSetupHs pkgDir
       ; return $ execWriter do
          { let setupArgs = [ setupHs, "-o"
                            , pkgDir </> "Setup"
                            , "-package-db=" ++ tempPkgDbDir
                            , ghcVerbosity verbosity
                            ] ++ map unitIdArg setupDepIds
          ; logMessage verbosity Verbose $
              "Compiling Setup.hs for " <> pkgNameVer
          ; callProcess $
              CP { cwd          = "."
                 , prog         = ghcPath
                 , args         = setupArgs
                 , extraPATH    = []
               , extraEnvVars = [] }
          }
        }

-- | Find the @Setup.hs@/@Setup.lhs@ file to use,
-- or create one using @main = defaultMain@ if none exist.
findSetupHs :: FilePath -> IO FilePath
findSetupHs root = trySetupsOrUseDefault [ "Setup.hs", "Setup.lhs" ]
  where
    useDefaultSetupHs = do
        let path = root </> "Setup.hs"
        writeFile path defaultSetupHs
        return path

    try fname = do
        let path = root </> fname
        exists <- doesFileExist path
        return $ if exists then Just path else Nothing

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

-- | Return build steps to setup a unit, and build steps
-- to configure, build and and register the unit in the package database.
--
-- You can run the build script with 'runBuildScript', or you can
-- turn it into a shell script with 'script'.
--
-- Note: executing the build script will fail
-- if the unit has already been installed/registered.
buildUnit :: Verbosity
          -> Compiler  -- ^ which @ghc@ and @ghc-pkg@ executables to use
          -> PkgDbDirs
          -> PkgDir
          -> DestDir Canonicalised
               -- ^ installation directory structure
          -> Args      -- ^ extra @Setup configure@ arguments for this unit
          -> Args      -- ^ extra @ghc-pkg register@ arguments
          -> Map UnitId PlanUnit -- ^ all dependencies in the build plan
          -> ConfiguredUnit -- ^ the unit to build
          -> BuildScript
buildUnit verbosity
             ( Compiler { ghcPath, ghcPkgPath } )
             ( PkgDbDirs { tempPkgDbDir, finalPkgDbDir } )
             ( PkgDir { pkgNameVer, pkgDir } )
             ( DestDir { installDir, prefix, destDir } )
             userConfigureArgs userGhcPkgArgs
             plan unit
  = let compName = Text.unpack $ cabalComponent ( puComponentName unit )
        thisUnitId = Text.unpack (unUnitId $ Configured.puId unit)
        unitPrintableName
          | verbosity >= Verbose
          = pkgNameVer <> ":" <> compName
          | otherwise
          = compName
    in execWriter do
  {
    -- Configure
  ; let flagsArg = case puFlags unit of
          flags
            | flagSpecIsEmpty flags
            -> []
            | otherwise
            -> [ "--flags=" ++ Text.unpack (showFlagSpec flags) ]
        buildDir = pkgDir </> "dist" </> thisUnitId
        configureArgs = [ "--with-compiler", ghcPath
                        , "--prefix", prefix
                        , "--cid=" ++ Text.unpack (unUnitId $ Configured.puId unit)
                        , "--package-db=" ++ tempPkgDbDir
                        , "--exact-configuration"
                        , "--datasubdir=" ++ pkgNameVer
                        , "--builddir=" ++ buildDir
                        , setupVerbosity verbosity
                        ] ++ flagsArg
                          ++ userConfigureArgs
                          ++ map ( dependencyArg plan unit )
                              ( Configured.puDepends unit )
                        ++ [ buildTarget unit ]
        setupExe = pkgDir </> "Setup" <.> exe
  ; logMessage verbosity Verbose $
      "Configuring " <> unitPrintableName
  ; logMessage verbosity Debug $
      "Configure arguments:\n" <> unlines (map ("  " <>) configureArgs)
  ; callProcess $
      CP { cwd          = pkgDir
         , prog         = setupExe
         , args         = "configure" : configureArgs

           -- Add the output binary directory to PATH, to satisfy executable
           -- dependencies during the build.
         , extraPATH    = [ installDir </> "bin" ]

            -- Specify the data directories for all dependencies.
         , extraEnvVars =
             [ ( mangledPkgName depName <> "_datadir"
               , installDir </> Text.unpack (pkgNameVersion depName depVer) )
             | depUnitId <- Configured.puDepends unit
             , let dep     = lookupDependency unit depUnitId plan
                   depName = planUnitPkgName dep
                   depVer  = planUnitVersion dep
            ]
         }

  -- Build
  ; logMessage verbosity Verbose $
      "Building " <> unitPrintableName
  ; callProcess $
      CP { cwd          = pkgDir
         , prog         = setupExe
         , args         = [ "build"
                          , "--builddir=" ++ buildDir
                          , setupVerbosity verbosity]
         , extraPATH    = []
         , extraEnvVars = [] }

   -- Copy
   ; logMessage verbosity Verbose $
       "Copying " <> unitPrintableName
   ; callProcess $
       CP { cwd          = pkgDir
          , prog         = setupExe
          , args         = [ "copy", setupVerbosity verbosity
                           , "--builddir=" ++ buildDir
                           , "--destdir", destDir ]
          , extraPATH    = []
          , extraEnvVars = [] }

      -- Register
   ;  case cuComponentType unit of
   { Lib -> do {
    -- Register library (in both the local and final package databases)
    -- See Note [Using two package databases] in Build.
    ; let pkgRegsFile = pkgDir </> ( thisUnitId <> "-pkg-reg.conf" )
          dirs = [ ( tempPkgDbDir, "temporary", ["--inplace"], [])
                 , (finalPkgDbDir, "final"    , [], "--force" : userGhcPkgArgs) ]

    ; for_ dirs \ (pkgDbDir, desc, extraSetupArgs, extraPkgArgs) ->

     do { logMessage verbosity Verbose $
           mconcat [ "Registering ", unitPrintableName, " in "
                   , desc, " package database at:\n  "
                   , pkgDbDir ]

          -- Setup register
        ; callProcess $
            CP { cwd          = pkgDir
               , prog         = setupExe
               , args         = [ "register", setupVerbosity verbosity
                                , "--builddir=" ++ buildDir
                                , "--gen-pkg-config=" ++ pkgRegsFile
                                ] ++ extraSetupArgs
               , extraPATH    = []
               , extraEnvVars = [] }

          -- ghc-pkg register
        ; callProcess $
            CP { cwd          = "."
               , prog         = ghcPkgPath
               , args         = [ "register"
                                , ghcPkgVerbosity verbosity
                                , "--package-db", pkgDbDir
                                , pkgRegsFile ]
                                ++ extraPkgArgs
               , extraPATH    = []
               , extraEnvVars = [] }
            -- NB: we have configured & built a single target,
            -- so there should be a single "pkg-reg.conf" file.
        } }

    ; _notALib -> return ()
    }

    ; logMessage verbosity Normal $ "Installed " <> unitPrintableName
    }

ghcVerbosity, ghcPkgVerbosity, setupVerbosity :: Verbosity -> String
ghcVerbosity (Verbosity i)
  | i <= 1
  = "-v0"
  | otherwise
  = "-v1"
ghcPkgVerbosity = ghcVerbosity
setupVerbosity  = ghcVerbosity

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

{- Note [Using two package databases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need *two* distinct package databases: we might want to perform the build
in a temporary location, before everything gets placed into its final
destination. The final package database might use a specific, baked-in
installation prefix (in the sense of @Setup configure --prefix <pfx>@). As a
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
data PkgDbDirs
  = PkgDbDirs
    { tempPkgDbDir  :: FilePath
        -- ^ Local package database directory
    , finalPkgDbDir :: FilePath
        -- ^ Installation package database directory
    }

getPkgDbDirs :: FilePath -- ^ fetched sources directory
             -> FilePath -- ^ installation directory
             -> PkgDbDirs
getPkgDbDirs fetchDir installDir =
  PkgDbDirs { tempPkgDbDir, finalPkgDbDir }
    where
      tempPkgDbDir  = fetchDir   </> "package.conf"
      finalPkgDbDir = installDir </> "package.conf"

-- | The package @name-version@ string and its directory.
data PkgDir
  = PkgDir
    { pkgNameVer    :: String
        -- ^ Package @name-version@ string
    , pkgDir        :: FilePath
        -- ^ Package directory
    }

-- | Compute some directory paths relevant to installation and registration
-- of a package.
getPkgDir :: FilePath       -- ^ fetched sources directory
          -> ConfiguredUnit -- ^ any unit from the package in question
          -> PkgDir
getPkgDir fetchDir ( ConfiguredUnit { puPkgName, puVersion, puPkgSrc } ) =
  PkgDir { pkgNameVer, pkgDir }
    where
      pkgNameVer = Text.unpack $ pkgNameVersion puPkgName puVersion
      pkgDir
        | Local dir <- puPkgSrc
        = dir
        | otherwise
        = fetchDir </> pkgNameVer
