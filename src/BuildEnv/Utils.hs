{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  BuildEnv.Utils
-- Description :  Utilities for @build-env@
--
-- Utilities for spawning of processes in particular environments.
--
-- See 'callProcessInIO'.
module BuildEnv.Utils
    ( -- * Call a process in a given environment
      CallProcess(..)
    , callProcessInIO

      -- * Create temporary directories
    , TempDirPermanence(..)
    , withTempDir

      -- * Abstract semaphores
    , AbstractSem(..)
    , withNewAbstractSem
    , noSem, abstractQSem

    ) where

-- base
import Control.Concurrent.QSem
  ( QSem, newQSem, signalQSem, waitQSem )
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

-- directory
import System.Directory
  ( withCurrentDirectory )

-- process
import qualified System.Process as Proc

#if BUILDENV_ENABLE_JSEM
-- base
import Control.Monad
  ( when )

-- semaphore-compat
import qualified System.Semaphore as System
  ( Semaphore(..), SemaphoreName(..)
  , freshSemaphore, openSemaphore
  , destroySemaphore
  , waitOnSemaphore, releaseSemaphore
  )
#endif

-- temporary
import System.IO.Temp
    ( createTempDirectory
    , getCanonicalTemporaryDirectory
    , withSystemTempDirectory
    )

-- build-env
import BuildEnv.Config
  ( Args, AsyncSem(..)
  , TempDirPermanence(..)
  , pATHSeparator, hostStyle
  )

--------------------------------------------------------------------------------

-- | Arguments to 'callProcess'.
data CallProcess
  = CP
  { cwd          :: FilePath
     -- ^ working directory
     --
     -- NB: this @cwd@ applies __before__ executing 'prog'.
     -- This is different from the @cwd@ of 'System.Process.createProcess'.
  , extraPATH    :: [FilePath]
     -- ^ filepaths to add to PATH
  , extraEnvVars :: [(String, String)]
     -- ^ extra environment variables
  , prog         :: FilePath
     -- ^ executable
  , args         :: Args
     -- ^ arguments
  , sem          :: AbstractSem
     -- ^ lock to take when calling the process
     -- and waiting for it to return, to avoid
     -- contention in concurrent situations
  }

-- | Run a command and wait for it to complete.
--
-- Crashes if the process returns with non-zero exit code.
--
-- See 'CallProcess' for a description of the options.
callProcessInIO :: HasCallStack => CallProcess -> IO ()
callProcessInIO ( CP { cwd, extraPATH, extraEnvVars, prog, args, sem } ) =
  withCurrentDirectory cwd do
    env <-
      if null extraPATH && null extraEnvVars
      then return Nothing
      else do env0 <- getEnvironment
              let env1 = Map.fromList $ env0 ++ extraEnvVars
                  env2 = Map.toList $ augmentSearchPath "PATH" extraPATH env1
              return $ Just env2
    let processArgs =
          ( Proc.proc prog args )
            { Proc.cwd = Nothing -- CWD set from the outside
            , Proc.env = env }
    res <- withAbstractSem sem do
      (_, _, _, ph) <- Proc.createProcess processArgs
      Proc.waitForProcess ph
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
    pathsVal = intercalate (pATHSeparator hostStyle) paths
    f Nothing  = Just pathsVal
    f (Just p) = Just (p <> (pATHSeparator hostStyle) <> pathsVal)

-- | Perform an action with a fresh temporary directory.
withTempDir :: TempDirPermanence  -- ^ whether to delete the temporary directory
                                  -- after the action completes
            -> String             -- ^ directory name template
            -> (FilePath -> IO a) -- ^ action to perform
            -> IO a
withTempDir del name k =
  case del of
    DeleteTempDirs
      -> withSystemTempDirectory name k
    Don'tDeleteTempDirs
      -> do root <- getCanonicalTemporaryDirectory
            createTempDirectory root name >>= k

--------------------------------------------------------------------------------
-- Semaphores.

-- | Abstract acquire/release mechanism.
newtype AbstractSem =
  AbstractSem { withAbstractSem :: forall r. IO r -> IO r }

-- | Create a semaphore-based acquire/release mechanism.
withNewAbstractSem :: AsyncSem
                   -> ( AbstractSem -> Args -> IO r )
                      -- ^ the abstract semaphore to use, and extra
                      -- arguments to pass to @Setup configure@ for @ghc@
                   -> IO r
withNewAbstractSem whatSem f =
  case whatSem of
    NoSem -> f noSem []
    NewQSem n -> do
      qsem <- newQSem ( fromIntegral n )
      f ( abstractQSem qsem ) []
#if BUILDENV_ENABLE_JSEM
    NewJSem n -> do
      jsem <- System.freshSemaphore "buildEnvSemaphore" ( fromIntegral n )
      let jsemName = System.semaphoreName jsem
      r <- f ( abstractJSem jsem ) [ jsemGhcArg jsemName ]
      System.destroySemaphore jsem
      return r
    ExistingJSem jsemName -> do
      jsem <- System.openSemaphore jsemName
      f ( abstractJSem jsem ) [ jsemGhcArg jsemName ]
  where
    jsemGhcArg :: System.SemaphoreName -> String
    jsemGhcArg ( System.SemaphoreName jsemName ) =
      "--ghc-option=-jsem=" <> jsemName
#endif

-- | No acquire/release mechanism required.
noSem :: AbstractSem
noSem = AbstractSem { withAbstractSem = id }

-- | Abstract acquire/release mechanism controlled by the given 'QSem'.
abstractQSem :: QSem -> AbstractSem
abstractQSem sem =
  AbstractSem \ mr -> do
    waitQSem sem
    r <- mr
    signalQSem sem
    return r

#if BUILDENV_ENABLE_JSEM
-- | Abstract acquire/release mechanism controlled by the given
-- system semaphore.
abstractJSem :: System.Semaphore -> AbstractSem
abstractJSem sem =
  AbstractSem \ mr -> do
    waitSucceeded <- System.waitOnSemaphore sem
    r <- mr
    when waitSucceeded $ System.releaseSemaphore sem 1
    return r
#endif
