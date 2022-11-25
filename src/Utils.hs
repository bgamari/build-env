{-# LANGUAGE CPP #-}

module Utils
    ( callProcessIn
    , withTempDir
    , exe
    ) where

-- base
import System.Exit
  ( ExitCode(..) )
import GHC.Stack
  ( HasCallStack )

-- process
import System.Process

-- temporary
import System.IO.Temp
    ( createTempDirectory
    , getCanonicalTemporaryDirectory
    , withSystemTempDirectory )

--------------------------------------------------------------------------------

-- | Run a command inside the specified working directory.
--
-- Crashes if the spawned command returns with nonzero exit code.
callProcessIn :: HasCallStack
              => FilePath -- ^ working directory
              -> FilePath -- ^ executable
              -> [String] -- ^ arguments
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

-- | Set this to 'True' to prevent deletion of temporary directories.
--
-- Useful for debugging this library.
noDelete :: Bool
noDelete = False

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name k
  | noDelete  = do
       root <- getCanonicalTemporaryDirectory
       createTempDirectory root name >>= k
  | otherwise = withSystemTempDirectory name k

-- | OS-dependent executable file extension.
exe :: String
exe =
#if defined(mingw32_HOST_OS)
  "exe"
#else
  ""
#endif
