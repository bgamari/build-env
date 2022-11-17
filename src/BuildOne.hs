module BuildOne
    ( Compiler(..)
    , buildPackage
    ) where

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

import Utils
import CabalPlan

data Compiler = Compiler { ghcPath :: FilePath
                         , ghcPkgPath :: FilePath
                         }

buildPackage :: FilePath -- ^ source directory
             -> ComponentName
             -> FilePath -- ^ installation prefix
             -> Compiler -- ^ compiler
             -> FlagSpec -- ^ flags
             -> IO ()
buildPackage srcDir component installDir comp flags = withSystemTempDirectory "build" $ \dir -> do
    setupHs <- findSetupHs srcDir
    let pkgDbDir = installDir </> "package.conf"
    createDirectoryIfMissing True pkgDbDir
    -- TODO: Handle setup-depends
    callProcessIn "." (ghcPath comp) [setupHs, "-o", srcDir </> "Setup"]
    let configureArgs = [ "--prefix", installDir
                        , "--flags=" ++ showFlagSpec flags
                        , "--package-db=" ++ pkgDbDir
                        ] ++ if component /= ComponentName (T.pack "lib")
                                then [T.unpack $ getComponentName component]
                                else []
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

