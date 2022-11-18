module Utils
    ( callProcessIn
    , withTempDir
    ) where

import System.IO.Temp
import System.Process
import System.Exit

callProcessIn :: FilePath -- ^ working directory
              -> FilePath -- ^ executable
              -> [String] -- ^ arguments
              -> IO ()
callProcessIn cwd exe args = do
    (_, _, _, ph) <- createProcess $ (proc exe args) { cwd = Just cwd }
    ExitSuccess <- waitForProcess ph
    return ()

-- | Don't delete temporary directories.
noDelete :: Bool
noDelete = False

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name k
  | noDelete  = do 
       root <- getCanonicalTemporaryDirectory
       createTempDirectory root name >>= k
  | otherwise = withSystemTempDirectory name k
