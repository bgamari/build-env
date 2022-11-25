module BuildOne ( buildPackage ) where

-- base
import Control.Monad ( guard )

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
  ( Compiler(..) )
import Utils
  ( callProcessIn, exe )
import CabalPlan
import qualified CabalPlan as Configured
  ( ConfiguredUnit(..) )

--------------------------------------------------------------------------------

buildPackage :: Compiler -- ^ compiler
             -> FilePath -- ^ source directory
             -> FilePath -- ^ installation prefix
             -> CabalPlan
             -> ConfiguredUnit
             -> IO ()
buildPackage comp srcDir installDir plan unit = do
    setupHs <- findSetupHs srcDir
    let pkgDbDir = installDir </> "package.conf"
    createDirectoryIfMissing True pkgDbDir
    let setupArgs = [ setupHs, "-o"
                    , srcDir </> "Setup"
                    , "-package-db=" ++ pkgDbDir
                    ] ++ (map packageId $ puSetupDepends unit)
    callProcessIn "." (ghcPath comp) setupArgs
    let configureArgs = [ "--prefix", installDir
                        , "--flags=" ++ showFlagSpec (puFlags unit)
                        , "--cid=" ++ Text.unpack (unPkgId $ Configured.puId unit)
                        , "--package-db=" ++ pkgDbDir
                        , "--exact-configuration"
                        ] ++ ( map (dependency plan unit) $ Configured.puDepends unit )
                        ++ [Text.unpack $ unComponentName $ puComponentName unit]
        setupExe = srcDir </> "Setup" <.> exe
    putStrLn $ "configure arguments: " ++ show configureArgs
    callProcessIn srcDir setupExe $ ["configure"] ++ configureArgs
    callProcessIn srcDir setupExe ["build"]
    callProcessIn srcDir setupExe ["copy"]
    let pkgRegsFile = "pkg-reg.conf"
        pkgRegDir = srcDir </> pkgRegsFile
    callProcessIn srcDir setupExe ["register", "--gen-pkg-config=" ++ pkgRegsFile]
    is_dir <- doesDirectoryExist pkgRegDir
    regFiles <-
        if is_dir
        then map (pkgRegDir </>) <$> listDirectory pkgRegDir
        else return [pkgRegDir]
    let register regFile =
          callProcessIn srcDir (ghcPkgPath comp) ["register", "--package-db", pkgDbDir, regFile]
    mapM_ register regFiles

packageId :: PkgId -> String
packageId (PkgId pkgId) = "-package-id " ++ Text.unpack pkgId

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
    mkDependency ( PU_Configured ( ConfiguredUnit { puPkgName = PkgName nm, puComponentName = ComponentName comp } ) )
      = Text.unpack comp ++ "=" ++ Text.unpack (unPkgId pkgId)


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
