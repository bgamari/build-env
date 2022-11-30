
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
  ( callProcessIn, exe )
import CabalPlan
import qualified CabalPlan as Configured
  ( ConfiguredUnit(..) )

--------------------------------------------------------------------------------

-- | Build a single unit and register it in the package database.
--
-- Note: this function will fail if the unit has already been registered.
buildPackage :: Verbosity
             -> Compiler  -- ^ which @ghc@ and @ghc-pkg@ executables to use
             -> FilePath  -- ^ directory of fetched sources
             -> FilePath  -- ^ installation directory (will be created if missing)
             -> [String]  -- ^ extra @setup configure@ arguments for this unit
                          -- (use this to specify haddock, hsc2hs, etc)
             -> CabalPlan -- ^ build plan to follow (used to specify dependencies)
             -> ConfiguredUnit -- ^ the unit to build
             -> IO ()
buildPackage verbosity ( Compiler { ghcPath, ghcPkgPath } )
             srcDir installDir
             userConfigureArgs
             plan unit = do
    let target = buildTarget unit
        printableName = Text.unpack $ componentName $ puComponentName unit
    normalMsg verbosity $ "Building " <> printableName
    setupHs <- findSetupHs srcDir
    let pkgDbDir = installDir </> "package.conf"
    createDirectoryIfMissing True pkgDbDir
    let setupArgs = [ setupHs, "-o"
                    , srcDir </> "Setup"
                    , "-package-db=" ++ pkgDbDir
                    , ghcVerbosity verbosity
                    ] ++ (map unitIdArg $ puSetupDepends unit)
    verboseMsg verbosity $
      "Compiling Setup.hs for " <> printableName
    callProcessIn "." ghcPath setupArgs
    let flagsArg = case puFlags unit of
          flags
            | flagSpecIsEmpty flags
            -> []
            | otherwise
            -> [ "--flags=" ++ Text.unpack (showFlagSpec (puFlags unit)) ]
        configureArgs = [ "--with-compiler", ghcPath
                        , "--prefix", installDir
                        , "--cid=" ++ Text.unpack (unUnitId $ Configured.puId unit)
                        , "--package-db=" ++ pkgDbDir
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
    verboseMsg verbosity $
      "Building " <> printableName
    callProcessIn srcDir setupExe ["build", setupVerbosity verbosity]
    verboseMsg verbosity $
      "Copying " <> printableName
    callProcessIn srcDir setupExe ["copy", setupVerbosity verbosity]
    let pkgRegsFile = "pkg-reg.conf"
        pkgRegDir = srcDir </> pkgRegsFile
    verboseMsg verbosity $
      "Creating package registration for " <> printableName
    callProcessIn srcDir setupExe [ "register", setupVerbosity verbosity
                                  , "--gen-pkg-config=" ++ pkgRegsFile ]
    is_dir <- doesDirectoryExist pkgRegDir
    regFiles <-
        if is_dir
        then map (pkgRegDir </>) <$> listDirectory pkgRegDir
        else return [pkgRegDir]
    let register regFile =
          callProcessIn srcDir ghcPkgPath
            [ "register"
            , ghcPkgVerbosity verbosity
            , "--package-db", pkgDbDir
            , regFile ]
    verboseMsg verbosity $
      "Registering " <> printableName <> " into '" <> pkgRegDir <> "'"
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
