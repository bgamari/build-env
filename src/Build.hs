{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Build where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Version
import System.Directory
import System.FilePath
import System.IO.Temp
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import BuildOne
import CabalPlan
import Utils

data Constraint = Constraint

data PkgSpec = PkgSpec { psName :: PkgName
                       , psConstraints :: Maybe Constraint
                       , psFlags :: FlagSpec
                       }

cabalPath = "cabal"

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

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    plan <- getPlan
        [ PkgSpec { psName = PkgName "filepath", psConstraints = Nothing, psFlags = mempty }
        ]
    print plan

    let comp = Compiler "ghc" "ghc-pkg"
    installDir <- canonicalizePath "install"
    buildPackage "pretty-1.1.3.6" installDir comp mempty

