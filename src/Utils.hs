module Utils
    ( callProcessIn
    ) where

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

