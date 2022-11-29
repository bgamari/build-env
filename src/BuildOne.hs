
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

-- | Build a single package and register it in the package database.
buildPackage :: Verbosity
             -> Compiler -- ^ which @ghc@ and @ghc-pkg@ executables to use
             -> FilePath -- ^ directory of fetched sources
             -> FilePath -- ^ installation directory (will be created if missing)
             -> CabalPlan
             -> ConfiguredUnit
             -> IO ()
buildPackage verbosity comp srcDir installDir plan unit = do
    let nmVersion = Text.unpack $
                    pkgNameVersion
                      (Configured.puPkgName unit)
                      (Configured.puVersion unit)
    normalMsg verbosity $ "Building " <> nmVersion
    setupHs <- findSetupHs srcDir
    let pkgDbDir = installDir </> "package.conf"
    createDirectoryIfMissing True pkgDbDir
    let setupArgs = [ setupHs, "-o"
                    , srcDir </> "Setup"
                    , "-package-db=" ++ pkgDbDir
                    , ghcVerbosity verbosity
                    ] ++ (map packageId $ puSetupDepends unit)
    verboseMsg verbosity $
      "Compiling Setup.hs for " <> nmVersion
    callProcessIn "." (ghcPath comp) setupArgs
    let configureArgs = [ "--prefix", installDir
                        , "--flags=" ++ Text.unpack (showFlagSpec (puFlags unit))
                        , "--cid=" ++ Text.unpack (unPkgId $ Configured.puId unit)
                        , "--package-db=" ++ pkgDbDir
                        , "--exact-configuration"
                        , setupVerbosity verbosity
                        ] ++ ( map (dependency plan unit) $ Configured.puDepends unit )
                        ++ [ buildTarget unit ]
        setupExe = srcDir </> "Setup" <.> exe
    verboseMsg verbosity $
      "Configuring " <> nmVersion
    callProcessIn srcDir setupExe $ ["configure", setupVerbosity verbosity] ++ configureArgs
    verboseMsg verbosity $
      "Building " <> nmVersion
    callProcessIn srcDir setupExe ["build", setupVerbosity verbosity]
    verboseMsg verbosity $
      "Copying " <> nmVersion
    callProcessIn srcDir setupExe ["copy", setupVerbosity verbosity]
    let pkgRegsFile = "pkg-reg.conf"
        pkgRegDir = srcDir </> pkgRegsFile
    verboseMsg verbosity $
      "Creating package registration for " <> nmVersion
    callProcessIn srcDir setupExe [ "register", setupVerbosity verbosity
                                  , "--gen-pkg-config=" ++ pkgRegsFile ]
    is_dir <- doesDirectoryExist pkgRegDir
    regFiles <-
        if is_dir
        then map (pkgRegDir </>) <$> listDirectory pkgRegDir
        else return [pkgRegDir]
    let register regFile =
          callProcessIn srcDir (ghcPkgPath comp)
            [ "register"
            , ghcPkgVerbosity verbosity
            , "--package-db", pkgDbDir, regFile]
    verboseMsg verbosity $
      "Registering " <> nmVersion <> " into '" <> pkgRegDir <> "'"
    mapM_ register regFiles
    normalMsg verbosity $ "Installed " <> nmVersion

ghcVerbosity, ghcPkgVerbosity, setupVerbosity :: Verbosity -> String
ghcVerbosity (Verbosity i)
  | i <= 1
  = "-v0"
  | otherwise
  = "-v1"
ghcPkgVerbosity = ghcVerbosity
setupVerbosity = ghcVerbosity

packageId :: PkgId -> String
packageId (PkgId pkgId) = "-package-id " ++ Text.unpack pkgId

buildTarget :: ConfiguredUnit -> String
buildTarget unit
  | ':' `elem` tgt
  = tgt
  | otherwise
  = "lib:" <> tgt
  where
    tgt = Text.unpack $ unComponentName $ puComponentName unit

-- | Create the text of a @--dependency=PKG:COMP:PKGID@ flag to specify
-- the package ID of a dependency to the configure script.
dependency :: CabalPlan -> ConfiguredUnit -> PkgId -> String
dependency fullPlan unit pkgId = "--dependency=" ++ mkDependency pu
  where
    pu :: PlanUnit
    pu = case mapMaybePlanUnits ( \ u -> do { guard ( pkgId == planUnitId u) ; return u }) fullPlan of
          [] -> error $ "buildPackage: can't find dependency in build plan\n\
                        \unit:" ++ show (Configured.puPkgName unit) ++ "\n\
                        \dependency:" ++ show pkgId
          cu:_ -> cu
    mkDependency :: PlanUnit -> String
    mkDependency ( PU_Preexisting ( PreexistingUnit { puPkgName = PkgName nm }))
      = Text.unpack nm ++ "=" ++ Text.unpack (unPkgId pkgId)
    mkDependency ( PU_Configured ( ConfiguredUnit { puComponentName = ComponentName comp } ) )
      = Text.unpack comp ++ "=" ++ Text.unpack (unPkgId pkgId)

-- | Find the @Setup.hs@ file to use, or create one using @main = defaultMain@
-- if none exist.
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
