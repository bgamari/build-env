{-# LANGUAGE CPP #-}

-- |
-- Module      :  Utils
-- Description :  Utilities for @build-env@.
module Utils
    ( callProcessIn
    , TempDirPermanence(..), withTempDir
    , exe
    ) where

-- base
import System.Exit
  ( ExitCode(..) )
import GHC.Stack
  ( HasCallStack )

-- process
import System.Process
    ( CreateProcess(cwd)
    , proc, createProcess, waitForProcess
    )

-- temporary
import System.IO.Temp
    ( createTempDirectory
    , getCanonicalTemporaryDirectory
    , withSystemTempDirectory
    )

-- build-env
import Config
  ( Args, TempDirPermanence(..) )

--------------------------------------------------------------------------------

-- | Run a command inside the specified working directory.
--
-- Crashes if the spawned command returns with nonzero exit code.
callProcessIn :: HasCallStack
              => FilePath -- ^ working directory
              -> FilePath -- ^ executable
              -> Args     -- ^ arguments
              -> IO ()
callProcessIn cwd prog args = do
    (_, _, _, ph) <- createProcess $ (proc prog args) { cwd = Just cwd }
    res <- waitForProcess ph
    case res of
      ExitSuccess -> return ()
      ExitFailure i -> do
        let argsStr
             | null args = ""
             | otherwise = " " ++ unwords args
        error ("command failed with exit code " ++ show i ++ ":\n"
              ++ "  > " ++ prog ++ argsStr )

withTempDir :: TempDirPermanence -> String -> (FilePath -> IO a) -> IO a
withTempDir del name k =
  case del of
    DeleteTempDirs
      -> withSystemTempDirectory name k
    Don'tDeleteTempDirs
      -> do root <- getCanonicalTemporaryDirectory
            createTempDirectory root name >>= k

-- | OS-dependent executable file extension.
exe :: String
exe =
#if defined(mingw32_HOST_OS)
  "exe"
#else
  ""
#endif
