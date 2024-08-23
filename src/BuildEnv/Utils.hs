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
      Program(..), programPath
    , CallProcess(..), callProcessInIO

      -- * Create temporary directories
    , TempDirPermanence(..)
    , withTempDir

      -- * Abstract semaphores
    , AbstractSem(..)
    , withNewAbstractSem, abstractQSem

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

-- fileloc
import qualified System.FileLock as FileLock
  ( SharedExclusive(..), withFileLock )

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
  , Compiler(..), Cabal(..)
  , Counter(..)
  , TempDirPermanence(..)
  , ProcessEnv(..)
  , pATHSeparator, hostStyle
  )
import BuildEnv.Path

--------------------------------------------------------------------------------

-- | A program used by @build-env@.
type Program :: Type -> Type
data Program dir
  -- | The @cabal@ program (@cabal-install@)
  = CabalProgram
  -- | The @ghc@ program.
  | GhcProgram
  -- |The @ghc-pkg@ program.
  | GhcPkgProgram
  -- | The Setup executable of a package.
  | SetupProgram ( RelativePath dir File )

programPath :: Compiler -> Cabal -> Program dir -> SymbolicPath dir File
programPath compiler cabal = \case
  CabalProgram      -> absoluteSymbolicPath $ cabalPath  cabal
  GhcProgram        -> absoluteSymbolicPath $ ghcPath    compiler
  GhcPkgProgram     -> absoluteSymbolicPath $ ghcPkgPath compiler
  SetupProgram path -> relativeSymbolicPath path

absoluteProgramPath :: SymbolicPath CWD ( Dir dir )
                    -> Compiler -> Cabal
                    -> Program dir
                    -> IO ( AbsolutePath File )
absoluteProgramPath workDir compiler cabal = \case
  CabalProgram      -> return $ cabalPath  cabal
  GhcProgram        -> return $ ghcPath    compiler
  GhcPkgProgram     -> return $ ghcPkgPath compiler
  SetupProgram path -> makeAbsolute workDir $ relativeSymbolicPath path


-- | Arguments to 'callProcess'.
data CallProcess dir
  = forall to
  . CP
  { prog         :: !( Program dir )
     -- ^ The program to run.
  , args         :: !Args
     -- ^ Arguments to the program.
  , lockFile     :: !( Maybe ( AbsolutePath to ) )
     -- ^ Optional file or directory to lock for this process invocation.
  }

-- | Run a command and wait for it to complete.
--
-- Crashes if the process returns with non-zero exit code.
--
-- See 'CallProcess' for a description of the options.
callProcessInIO :: HasCallStack
                => Compiler
                -> Cabal
                -> ProcessEnv dir
                -> Maybe Counter
                    -- ^ Optional counter. Used when the command fails,
                    -- to report the progress that has been made so far.
                -> CallProcess dir
                -> IO ()
callProcessInIO compiler cabal ( ProcessEnv workDir extraPATH extraEnvVars logBasePath ) mbCounter
  ( CP { prog, args, lockFile } ) = do
    absProgPath <- absoluteProgramPath workDir compiler cabal prog
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
          [ "  > " ++ show absProgPath ++ argsStr
          , "  CWD = " ++ show workDir ]
    env <-
      if null extraPATH && null extraEnvVars
      then return Nothing
      else do env0 <- getEnvironment
              let env1 = Map.fromList $ env0 ++ extraEnvVars
                  env2 = Map.toList $ augmentSearchPath "PATH" extraPATH env1
              return $ Just env2
    let withHandles :: ( ( Proc.StdStream, Proc.StdStream ) -> IO () ) -> IO ()
        withHandles action =
          case logBasePath of
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
            ( Proc.proc ( getAbsolutePath absProgPath ) args )
              { Proc.cwd     = if getSymbolicPath workDir == "." then Nothing else Just ( getSymbolicPath workDir )
              , Proc.env     = env
              , Proc.std_out = stdoutStream
              , Proc.std_err = stderrStream }
      res <- withMaybeFileLock lockFile do
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
            _ -> putStrLn $ unlines msg
          exitWith res

-- | Do something while taking a lock on a file or directory.
withMaybeFileLock :: Maybe ( AbsolutePath to ) -> IO a -> IO a
withMaybeFileLock mbLocked action = case mbLocked of
  Nothing     -> action
  Just locked -> FileLock.withFileLock ( getAbsolutePath locked <.> "lock" ) FileLock.Exclusive
               $ const action

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
