{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , TempDirPermanence(..)
    , withTempDir

      -- * Abstract semaphores
    , AbstractSem(..)
    , withNewAbstractSem, noSem, abstractQSem

      -- * Other utilities
    , splitOn

    ) where

-- base
import Control.Concurrent.QSem
  ( QSem, newQSem, signalQSem, waitQSem )
import Control.Exception
  ( bracket, bracket_ )
import Data.Kind
  ( Type )
import Data.List
  ( intercalate )
import Data.Maybe
  ( maybeToList )
import Data.IORef
  ( readIORef )
import System.Environment
  ( getEnvironment )
import System.Exit
  ( ExitCode(..), exitWith )
import System.IO
  ( IOMode(..), hPutStrLn, withFile )
import qualified System.IO as System.Handle
  ( stderr )
import GHC.IO.Handle
  ( hDuplicateTo )
import GHC.Stack
  ( HasCallStack )

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( alter, fromList, toList )

-- directory
import System.Directory
  ( createDirectoryIfMissing )

-- filepath
import System.FilePath
  ( takeDirectory )

-- process
import qualified System.Process as Proc

-- semaphore-compat
import qualified System.Semaphore as System
  ( Semaphore(..), SemaphoreName(..)
  , freshSemaphore, openSemaphore
  , destroySemaphore
  , waitOnSemaphore, releaseSemaphore
  )

-- temporary
import System.IO.Temp
    ( createTempDirectory
    , getCanonicalTemporaryDirectory
    , withSystemTempDirectory
    )

-- build-env
import BuildEnv.Config
  ( Args, AsyncSem(..)
  , Counter(..)
  , TempDirPermanence(..)
  , pATHSeparator, hostStyle
  )
import BuildEnv.Path

--------------------------------------------------------------------------------

-- | The path of a program to run.
type ProgPath :: Type -> Type
data ProgPath from
  -- | An absolute path, or an executable in @PATH@.
  = AbsPath { absProgPath :: !( AbsolutePath File ) }
  -- | A relative path, relative to the @from@ abstract location.
  | RelPath { relProgPath :: !( SymbolicPath from File ) }

-- | Arguments to 'callProcess'.
data CallProcess dir
  = CP
  { cwd          :: !( SymbolicPath CWD ( Dir dir ) )
     -- ^ Working directory.
  , extraPATH    :: ![ FilePath ]
     -- ^ Absolute filepaths to add to PATH.
  , extraEnvVars :: ![ ( String, String ) ]
     -- ^ Extra environment variables to add before running the command.
  , prog         :: !( ProgPath dir )
     -- ^ The program to run.
     --
     -- If it's a relative path, it should be relative to the @cwd@ field.
  , args         :: !Args
     -- ^ Arguments to the program.
  , logBasePath  :: !( Maybe ( AbsolutePath File ) )
     -- ^ Log @stdout@ to @basePath.stdout@ and @stderr@ to @basePath.stderr@.
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
                -> CallProcess dir
                -> IO ()
callProcessInIO mbCounter ( CP { cwd, extraPATH, extraEnvVars, prog, args, logBasePath, sem } ) = do
  absProg <-
    case prog of
      AbsPath p -> return p
      RelPath p -> makeAbsolute cwd p
        -- Needs to be an absolute path, as per the @process@ documentation:
        --
        --   If cwd is provided, it is implementation-dependent whether
        --   relative paths are resolved with respect to cwd or the current
        --   working directory, so absolute paths should be used
        --   to ensure portability.
        --
        -- We always want the program to be interpreted relative to the cwd
        -- argument, so we prepend @cwd@ and then make it absolute.
  let argsStr
        | null args = ""
        | otherwise = " " ++ unwords args
      command =
        [ "  > " ++ show absProg ++ argsStr
        , "  CWD = " ++ show cwd ]
  env <-
    if null extraPATH && null extraEnvVars
    then return Nothing
    else do env0 <- getEnvironment
            let env1 = Map.fromList $ env0 ++ extraEnvVars
                env2 = Map.toList $ augmentSearchPath "PATH" extraPATH env1
            return $ Just env2
  let withHandles :: ( ( Proc.StdStream, Proc.StdStream ) -> IO () ) -> IO ()
      withHandles action = case logBasePath of
        Nothing -> action ( Proc.Inherit, Proc.Inherit )
        Just logPath -> do
          let stdoutFile = logPath <.> "stdout"
              stderrFile = logPath <.> "stderr"
          createDirectoryIfMissing True $ takeDirectory $ getAbsolutePath logPath
          withFile ( getAbsolutePath stdoutFile ) AppendMode \ stdoutFileHandle ->
            withFile ( getAbsolutePath stderrFile ) AppendMode \ stderrFileHandle -> do
              hDuplicateTo System.Handle.stderr stderrFileHandle
                -- Write stderr to the log file and to the terminal.
              hPutStrLn stdoutFileHandle ( unlines command )
              action ( Proc.UseHandle stdoutFileHandle, Proc.UseHandle stderrFileHandle )
  withHandles \ ( stdoutStream, stderrStream ) -> do
    let processArgs =
          ( Proc.proc ( getAbsolutePath absProg ) args )
            { Proc.cwd     = if getSymbolicPath cwd == "." then Nothing else Just ( getSymbolicPath cwd )
            , Proc.env     = env
            , Proc.std_out = stdoutStream
            , Proc.std_err = stderrStream }
    res <- withAbstractSem sem do
      (_, _, _, ph) <- Proc.createProcess_ "createProcess" processArgs
        -- Use 'createProcess_' to avoid closing handles prematurely.
      Proc.waitForProcess ph
    case res of
      ExitSuccess -> return ()
      ExitFailure i -> do
        progressReport <-
          case mbCounter of
            Nothing -> return []
            Just ( Counter { counterRef, counterMax } ) -> do
              progress <- readIORef counterRef
              return $ [ "After " <> show progress <> " of " <> show counterMax ]
        let msg = [ "callProcess failed with non-zero exit code " ++ show i ++ ". Command:" ]
                     ++ command ++ progressReport
        case stderrStream of
          Proc.UseHandle errHandle ->
            hPutStrLn errHandle
              ( unlines $ msg ++ [ "Logs are available at: " <> getAbsolutePath logs <> ".{stdout, stderr}" | logs <- maybeToList logBasePath ] )
          _ -> putStrLn (unlines msg)
        exitWith res

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
            -> ( AbsolutePath ( Dir Tmp ) -> IO a ) -- ^ action to perform
            -> IO a
withTempDir del name k =
  case del of
    DeleteTempDirs
      -> withSystemTempDirectory name ( k . mkAbsolutePath )
    Don'tDeleteTempDirs
      -> do root <- getCanonicalTemporaryDirectory
            createTempDirectory root name >>= ( k . mkAbsolutePath )

-- | Utility list 'splitOn' function.
splitOn :: Char -> String -> [String]
splitOn c = go
  where
    go "" = []
    go s
      | (a,as) <- break (== c) s
      = a : go (drop 1 as)

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
    NewJSem n ->
      bracket
        ( System.freshSemaphore "buildEnvSemaphore" ( fromIntegral n ) )
        System.destroySemaphore
        $ \ jsem -> do
          let jsemName = System.semaphoreName jsem
          f ( abstractJSem jsem ) [ jsemGhcArg jsemName ]
    ExistingJSem jsemName -> do
      let jsemNm = System.SemaphoreName jsemName
      jsem <- System.openSemaphore jsemNm
      f ( abstractJSem jsem ) [ jsemGhcArg jsemNm ]
  where
    jsemGhcArg :: System.SemaphoreName -> String
    jsemGhcArg ( System.SemaphoreName jsemName ) =
      "--ghc-option=-jsem=" <> jsemName

-- | No acquire/release mechanism required.
noSem :: AbstractSem
noSem = AbstractSem { withAbstractSem = id }

-- | Abstract acquire/release mechanism controlled by the given 'QSem'.
abstractQSem :: QSem -> AbstractSem
abstractQSem sem =
  AbstractSem $
    bracket_
      ( waitQSem   sem )
      ( signalQSem sem )

-- | Abstract acquire/release mechanism controlled by the given
-- system semaphore.
abstractJSem :: System.Semaphore -> AbstractSem
abstractJSem sem =
  AbstractSem $
    bracket_
      ( System.waitOnSemaphore  sem )
      ( System.releaseSemaphore sem 1 )
