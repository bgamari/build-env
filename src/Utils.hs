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
import Data.List
  ( intercalate )
import System.Environment
  ( getEnvironment )
import System.Exit
  ( ExitCode(..) )
import GHC.Stack
  ( HasCallStack )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( alter, fromList, toList )

-- process
import System.Process
    ( CreateProcess(cwd, env)
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
              => FilePath   -- ^ working directory
              -> [FilePath] -- ^ filepaths to add to PATH
              -> FilePath   -- ^ executable
              -> Args       -- ^ arguments
              -> IO ()
callProcessIn cwd addToPath prog args = do
    let processArgs0 = (proc prog args) { cwd = Just cwd }
    processArgs <- case addToPath of
      [] -> return processArgs0
      _  -> do
        env <- Map.fromList <$> getEnvironment
        let env' = Map.toList $ augmentSearchPath "PATH" addToPath env
        return $ processArgs0 { env = Just env' }
    (_, _, _, ph) <- createProcess processArgs
    res <- waitForProcess ph
    case res of
      ExitSuccess -> return ()
      ExitFailure i -> do
        let argsStr
             | null args = ""
             | otherwise = " " ++ unwords args
        error ("command failed with exit code " ++ show i ++ ":\n"
              ++ "  > " ++ prog ++ argsStr )

-- | Add filepaths to the given key in a key/value environment.
augmentSearchPath :: Ord k => k -> [FilePath] -> Map k String -> Map k String
augmentSearchPath _   []    = id
augmentSearchPath var paths = Map.alter f var
  where
    pathsVal = intercalate pATHSeparator paths
    f Nothing  = Just pathsVal
    f (Just p) = Just (p <> pATHSeparator <> pathsVal)

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

-- | OS-dependent separator for PATH variable.
pATHSeparator :: String
pATHSeparator =
#if defined(mingw32_HOST_OS)
  ";"
#else
  ":"
#endif
