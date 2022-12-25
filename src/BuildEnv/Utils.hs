{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  BuildEnv.Utils
-- Description :  Utilities for @build-env@
--
-- Various utilities:
--
--  - Spawning of processes in particular environments; see 'callProcessInIO'.
--  - Semaphores.
--
module BuildEnv.Utils
    ( -- * Call a process in a given environment
      ProgPath(..), CallProcess(..), callProcessInIO

      -- * Create temporary directories
    , TempDirPermanence(..), withTempDir

      -- * Abstract semaphores
    , AbstractSem(..)
    , newAbstractSem, noSem, abstractQSem

    ) where

-- base
import Control.Concurrent.QSem
  ( QSem, newQSem, signalQSem, waitQSem )
import Control.Exception
  ( bracket_ )
import Data.List
  ( intercalate )
import Data.IORef
  ( readIORef )
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
  ( makeAbsolute )

-- filepath
import System.FilePath
  ( (</>) )

-- process
import qualified System.Process as Proc

-- temporary
import System.IO.Temp
    ( createTempDirectory
    , getCanonicalTemporaryDirectory
    , withSystemTempDirectory
    )

-- build-env
import BuildEnv.Config
  ( AsyncSem(..), Args, TempDirPermanence(..), Counter(..)
  , pATHSeparator, hostStyle
  )

--------------------------------------------------------------------------------

-- | The path of a program to run.
data ProgPath
  -- | An absolute path, or an executable in @PATH@.
  = AbsPath { progPath :: !FilePath }
  -- | A relative path. What it is relative to depends on context.
  | RelPath { progPath :: !FilePath }

-- | Arguments to 'callProcess'.
data CallProcess
  = CP
  { cwd          :: !FilePath
     -- ^ Working directory.
  , extraPATH    :: ![FilePath]
     -- ^ Absolute filepaths to add to PATH.
  , extraEnvVars :: ![(String, String)]
     -- ^ Extra envi!ronment variables to add before running the command.
  , prog         :: !ProgPath
     -- ^ The program to run.
     --
     -- If it's a relative path, it should be relative to the @cwd@ field.
  , args         :: !Args
     -- ^ Arguments to the program.
  , sem          :: !AbstractSem
     -- ^ Lock to take when calling the process
     -- and waiting for it to return, to avoid
     -- contention in concurrent situations.
  }

-- | Run a command and wait for it to complete.
--
-- Crashes if the process returns with non-zero exit code.
--
-- See 'CallProcess' for a description of the options.
callProcessInIO :: HasCallStack
                => Maybe Counter
                    -- ^ Optional counter. Used when the command fails,
                    -- to report the progress that has been made so far.
                -> CallProcess
                -> IO ()
callProcessInIO mbCounter ( CP { cwd, extraPATH, extraEnvVars, prog, args, sem } ) = do
  absProg <-
    case prog of
      AbsPath p -> return p
      RelPath p -> makeAbsolute $ cwd </> p
        -- Needs to be an absolute path, as per the @process@ documentation:
        --
        --   If cwd is provided, it is implementation-dependent whether
        --   relative paths are resolved with respect to cwd or the current
        --   working directory, so absolute paths should be used
        --   to ensure portability.
        --
        -- We always want the program to be interpreted relative to the cwd
        -- argument, so we prepend @cwd@ and then make it absolute.
  env <-
    if null extraPATH && null extraEnvVars
    then return Nothing
    else do env0 <- getEnvironment
            let env1 = Map.fromList $ env0 ++ extraEnvVars
                env2 = Map.toList $ augmentSearchPath "PATH" extraPATH env1
            return $ Just env2
  let processArgs =
        ( Proc.proc absProg args )
          { Proc.cwd = if cwd == "." then Nothing else Just cwd
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
      progressReport <-
        case mbCounter of
          Nothing -> return []
          Just ( Counter { counterRef, counterMax } ) -> do
            progress <- readIORef counterRef
            return $ [ "After " <> show progress <> " of " <> show counterMax ]
      error . unlines $
        [ "callProcess failed with non-zero exit code " ++ show i ++ "."
        , "  > " ++ absProg ++ argsStr
        , "CWD = " ++ cwd ] ++ progressReport

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
newAbstractSem :: AsyncSem -> IO AbstractSem
newAbstractSem whatSem =
  case whatSem of
    NoSem -> return noSem
    NewQSem n -> do
      qsem <- newQSem ( fromIntegral n )
      return $ abstractQSem qsem

-- | Abstract acquire/release mechanism controlled by the given 'QSem'.
abstractQSem :: QSem -> AbstractSem
abstractQSem sem =
  AbstractSem $ bracket_ (waitQSem sem) (signalQSem sem)

-- | No acquire/release mechanism required.
noSem :: AbstractSem
noSem = AbstractSem { withAbstractSem = id }
