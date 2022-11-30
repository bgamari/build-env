{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  BuildOne
-- Description :  Configure, build and install a single package
--
-- 'buildPackage' configures and builds a single package using the @Setup.hs@
-- script, before registering it into a local package database using @ghc-pkg@.
module BuildOne ( buildPackage ) where

-- base
import Control.Monad
  ( guard )
import Data.Foldable
  ( for_ )

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
import Config
import Utils
import CabalPlan
import qualified CabalPlan as Configured
  ( ConfiguredUnit(..) )

--------------------------------------------------------------------------------

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


-- | Build a single unit and register it in the package database.
--
-- Note: this function will fail if the unit has already been registered.
buildPackage :: Verbosity
             -> Compiler  -- ^ which @ghc@ and @ghc-pkg@ executables to use
             -> FilePath  -- ^ source directory for this specific unit
             -> FilePath  -- ^ (possibly temporary) build directory
             -> DestDir Canonicalised
                  -- ^ installation directory structure
             -> [String]  -- ^ extra @Setup configure@ arguments for this unit
             -> CabalPlan -- ^ build plan to follow (used to specify dependencies)
             -> ConfiguredUnit -- ^ the unit to build
             -> IO ()
buildPackage verbosity
             ( Compiler { ghcPath, ghcPkgPath } )
             srcDir tempBuildDir ( DestDir { prefix, destDir, installDir } )
             userConfigureArgs
             plan unit = do
    let target = buildTarget unit
        printableName = Text.unpack $ componentName $ puComponentName unit
    normalMsg verbosity $ "Building " <> printableName

    -- Setup
    setupHs <- findSetupHs srcDir
    let tempPkgDbDir  = tempBuildDir </> "package.conf"
        finalPkgDbDir = installDir   </> "package.conf"
          -- See Note [Using two package databases].
    mapM_ (createDirectoryIfMissing True) [ tempPkgDbDir, finalPkgDbDir ]
    let setupArgs = [ setupHs, "-o"
                    , srcDir </> "Setup"
                    , "-package-db=" ++ tempPkgDbDir
                    , ghcVerbosity verbosity
                    ] ++ (map unitIdArg $ puSetupDepends unit)
    verboseMsg verbosity $
      "Compiling Setup.hs for " <> printableName
    callProcessIn "." ghcPath setupArgs

    -- Configure
    let flagsArg = case puFlags unit of
          flags
            | flagSpecIsEmpty flags
            -> []
            | otherwise
            -> [ "--flags=" ++ Text.unpack (showFlagSpec (puFlags unit)) ]
        configureArgs = [ "--with-compiler", ghcPath
                        , "--prefix", prefix
                        , "--cid=" ++ Text.unpack (unUnitId $ Configured.puId unit)
                        , "--package-db=" ++ tempPkgDbDir
                        , "--exact-configuration"
                        , setupVerbosity verbosity
                        ] ++ flagsArg
                          ++ userConfigureArgs
                          ++ ( map (dependencyArg plan unit) $ Configured.puDepends unit )
                        ++ [ target ]
        setupExe = srcDir </> "Setup" <.> exe
    verboseMsg verbosity $
      "Configuring " <> printableName
    debugMsg verbosity $
      "Configure arguments:\n" <> unlines (map ("  " <>) configureArgs)
    callProcessIn srcDir setupExe $ ["configure"] ++ configureArgs

    -- Build
    verboseMsg verbosity $
      "Building " <> printableName
    callProcessIn srcDir setupExe ["build", setupVerbosity verbosity]

    -- Copy
    verboseMsg verbosity $
      "Copying " <> printableName
    callProcessIn srcDir setupExe $
      [ "copy", setupVerbosity verbosity
      , "--destdir", destDir ]

    -- Register (in both the local and final package databases)
    -- See Note [Using two package databases].
    let pkgRegsFile = "pkg-reg.conf"
        pkgRegDir = srcDir </> pkgRegsFile
        dirs = [ ( tempPkgDbDir, "temporary", ["--inplace"], [])
               , (finalPkgDbDir, "final"    , [], ["--force"]) ]
    for_ dirs \ (pkgDbDir, desc, extraSetupArgs, extraPkgArgs) -> do

      verboseMsg verbosity $
        mconcat [ "Registering ", printableName, " in "
                , desc, " package database at:\n  "
                , pkgDbDir ]

      -- Setup register
      callProcessIn srcDir setupExe $
        [ "register", setupVerbosity verbosity
        , "--gen-pkg-config=" ++ pkgRegDir
        ] ++ extraSetupArgs

      -- ghc-pkg register
      is_dir <- doesDirectoryExist pkgRegDir
      regFiles <-
          if is_dir
          then map (pkgRegDir </>) <$> listDirectory pkgRegDir
          else return [pkgRegDir]
      let register regFile =
            callProcessIn "." ghcPkgPath $
              [ "register"
              , ghcPkgVerbosity verbosity
              , "--package-db", pkgDbDir
              , regFile ]
              ++ extraPkgArgs
      mapM_ register regFiles


    normalMsg verbosity $ "Installed " <> printableName

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
buildTarget ( ConfiguredUnit { puComponentName = ComponentName ty nm } )
  = Text.unpack (ty <> ":" <> nm)

-- | The argument @--dependency=PKG:COMP=UNIT_ID@.
--
-- Used to specify the 'UnitId' of a dependency to the configure script.
-- This allows us to perform a build using the specific dependencies we have
-- available, ignoring any bounds in the cabal file.
dependencyArg :: CabalPlan -> ConfiguredUnit -> UnitId -> String
dependencyArg fullPlan unit unitId = "--dependency=" ++ Text.unpack (mkDependency pu)
  where
    pu :: PlanUnit
    pu = case mapMaybePlanUnits ( \ u -> do { guard ( unitId == planUnitId u) ; return u }) fullPlan of
          [] -> error $ "buildPackage: can't find dependency in build plan\n\
                        \unit:" ++ show (Configured.puPkgName unit) ++ "\n\
                        \dependency:" ++ show unitId
          cu:_ -> cu
    mkDependency :: PlanUnit -> Text
    mkDependency ( PU_Preexisting ( PreexistingUnit { puPkgName = PkgName nm }))
      = nm <> "=" <> unUnitId unitId
    mkDependency ( PU_Configured ( ConfiguredUnit { puPkgName = PkgName pkg
                                                  , puComponentName = ComponentName _ comp } ) )
      = pkg <> ":" <> comp <> "=" <> unUnitId unitId

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
