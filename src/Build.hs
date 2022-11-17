{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Build where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Version
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import CabalPlan

data Compiler = Compiler { ghcPath :: FilePath
                         , ghcPkgPath :: FilePath
                         }

data Constraint = Constraint

data PkgSpec = PkgSpec { psName :: PkgName
                       , psConstraints :: Maybe Constraint
                       , psFlags :: FlagSpec
                       }

cabalPath = "cabal"

callProcessIn :: FilePath -- ^ working directory
              -> FilePath -- ^ executable
              -> [String] -- ^ arguments
              -> IO ()
callProcessIn cwd exe args = do
    (_, _, _, ph) <- createProcess $ (proc exe args) { cwd = Just cwd }
    ExitSuccess <- waitForProcess ph
    return ()

getPlan :: [PkgSpec] -> IO CabalPlan
getPlan pkgs = withSystemTempDirectory "build" $ \dir -> do
    writeFile (dir </> "cabal.project") projectContents
    writeFile (dir </> "dummy-package.cabal") cabalContents
    callProcessIn dir cabalPath ["configure", "-v0"]
    mb_plan <- Aeson.eitherDecode <$> BSL.readFile ("dist-newstyle" </> "cache" </> "plan.json")
    case mb_plan of
      Left err -> fail err
      Right plan -> return plan
  where
    projectContents = unlines
        [ "packages: ."
        ] ++
        unlines
        [ unlines
          [ "package " <> T.unpack (unPkgName $ psName ps)
          , "  flags: " <> showFlagSpec (psFlags ps)
          ]
        | ps <- pkgs
        ]

    cabalContents = unlines
        [ "cabal-version: 2.4"
        , "name: dummy-package"
        , "version: 0"
        , "library"
        , "  build-depends:"
        ] ++ intercalate "," [ "    " <> T.unpack (unPkgName $ psName ps) | ps <- pkgs ]

buildPackage :: FilePath -- ^ source directory
             -> FilePath -- ^ installation prefix
             -> Compiler -- ^ compiler
             -> PkgSpec  -- ^ package
             -> IO ()
buildPackage srcDir installDir comp pkg = withSystemTempDirectory "build" $ \dir -> do
    setupHs <- findSetupHs srcDir
    let pkgDbDir = installDir </> "package.conf"
    createDirectoryIfMissing True pkgDbDir
    -- TODO: Handle setup-depends
    callProcessIn "." (ghcPath comp) [setupHs, "-o", srcDir </> "Setup"]
    let configureArgs = [ "--prefix", installDir
                        , "--flags=" ++ showFlagSpec (psFlags pkg)
                        ]
    callProcessIn srcDir "./Setup" $ ["configure"] ++ configureArgs
    callProcessIn srcDir "./Setup" ["build"]
    callProcessIn srcDir "./Setup" ["copy"]
    callProcessIn srcDir "./Setup" ["register", "--gen-pkg-config=tmp.conf"]
    callProcessIn srcDir (ghcPkgPath comp) [ "register", "--package-db", pkgDbDir, "tmp.conf"]

oneOf :: [IO (Maybe a)] -> IO (Maybe a)
oneOf = foldr f (return Nothing)
  where
    f k rest = do
        r <- k
        case r of
          Nothing -> rest
          Just x -> return $ Just x

findSetupHs :: FilePath -> IO FilePath
findSetupHs root = fromMaybe undefined <$> oneOf [ try "Setup.hs", try "Setup.lhs", populate ]
  where
    populate = do
        let path = root </> "Setup.hs"
        writeFile path defaultSetupHs
        return $ Just path

    try fname = do
        let path = root </> fname
        exists <- doesFileExist path
        return $ if exists then Just path else Nothing

    defaultSetupHs = unlines
        [ "import Distribution.Simple"
        , "main = defaultMain"
        ]

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    plan <- getPlan
        [ PkgSpec { psName = PkgName "filepath", psConstraints = Nothing, psFlags = mempty }
        ]
    print plan
    let comp = Compiler "ghc" "ghc-pkg"
    let ps = PkgSpec { psName = PkgName "pretty", psConstraints = Nothing, psFlags = mempty }
    installDir <- canonicalizePath "install"
    buildPackage "pretty-1.1.3.6" installDir comp ps

