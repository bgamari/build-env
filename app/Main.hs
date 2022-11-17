{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import System.Process
import System.IO.Temp
import System.FilePath
import Data.Aeson
import Data.Version
import Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL

newtype PkgName = PkgName { unPkgName :: T.Text }

data Constraint = Constraint

data PkgSpec = PkgSpec { psName :: PkgName
                       , psConstraints :: Maybe Constraint
                       , psFlags :: M.Map T.Text Bool
                       }

cabalPath = "cabal"

data Plan = Plan 
    deriving (Show)

instance FromJSON Plan where
    parseJSON = withObject "cabal plan" $ \o -> do
        return Plan

getPlan :: [PkgSpec] -> IO Plan
getPlan pkgs = withSystemTempDirectory "build" $ \dir -> do
    writeFile (dir </> "cabal.project") projectContents
    writeFile (dir </> "dummy-package.cabal") cabalContents
    (_, _, _, ph) <- createProcess $ (proc cabalPath ["configure"]) { cwd = Just dir }
    waitForProcess ph
    Just plan <- decode <$> BSL.readFile ("dist-newstyle" </> "cache" </> "plan.json")
    return plan
  where
    projectContents = unlines
        [ "packages: ."
        ]
    cabalContents = unlines
        [ "cabal-version: 2.4"
        , "name: dummy-package"
        , "version: 0"
        , "library"
        , "  build-depends:"
        ] ++ intercalate "," [ T.unpack $ unPkgName $ psName ps | ps <- pkgs ]

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    plan <- getPlan [ PkgSpec "filepath" Nothing mempty ]
    print plan
