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

-- | Don't delete temporary directories.
noDelete :: Bool
noDelete = True -- for debugging... otherwise False

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name k
  | noDelete  = do
       root <- getCanonicalTemporaryDirectory
       createTempDirectory root name >>= k
  | otherwise = withSystemTempDirectory name k

exe :: String
exe =
#if defined(mingw32_HOST_OS)
  "exe"
#else
  ""
#endif
