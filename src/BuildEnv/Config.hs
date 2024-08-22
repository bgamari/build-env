{-# LANGUAGE CPP #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  BuildEnv.Config
-- Description :  Configuration options for @build-env@
--
-- Configuration options for @build-env@
module BuildEnv.Config
  ( -- * Build strategy
    BuildStrategy(..), RunStrategy(..)
  , AsyncSem(..), semDescription

   -- * Passing arguments
  , Args, UnitArgs(..)

    -- * @ghc@ and @cabal-install@ executables
  , Compiler(..), Cabal(..)

    -- * Hackage index state
  , IndexState(..)

    -- * Directory structure
  , Paths(..), BuildPaths(..)
  , PathUsability(..)
  , canonicalizePaths

    -- ** Handling of temporary directories
  , TempDirPermanence(..)

    -- * Logging verbosity
  , Verbosity(.., Quiet, Normal, Verbose, Debug)
  , quietMsg, normalMsg, verboseMsg, debugMsg
  , ghcVerbosity, ghcPkgVerbosity, cabalVerbosity, setupVerbosity

    -- * Reporting progress
  , Counter(..)

    -- * OS specifics
  , Style(..), hostStyle
  , pATHSeparator

  ) where

-- base
import Control.Monad
  ( when )
import Data.Kind
  ( Type )
import Data.IORef
  ( IORef )
import Data.Word
  ( Word16 )
import System.IO
  ( hFlush, stdout )

-- filepath
import System.FilePath
  ( dropDrive )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( pack )
import qualified Data.Text.IO as Text
  ( putStrLn )

-- time
import Data.Time.Clock
  ( getCurrentTime )
import Data.Time.Format
  ( defaultTimeLocale, formatTime )

-- build-env
import BuildEnv.Path

--------------------------------------------------------------------------------
-- Build strategy

-- | Build strategy for 'BuildEnv.Build.buildPlan'.
data BuildStrategy
  -- | Execute the build plan in-place.
  = Execute RunStrategy
  -- | Output a build script that can be run later.
  | Script
    { scriptPath   :: !( SymbolicPath CWD File )
      -- ^ Output path at which to write the build script.
    , useVariables :: !Bool
      -- ^ Should the output shell script use variables, or baked in paths?
      --
      -- The shell script will use the following variables:
      --
      -- - @GHC@, @GHCPKG@, @SOURCES@, @PREFIX@, @DESTDIR@.
    }
  deriving stock Show

-- | How to execute a build plan.
data RunStrategy
  -- | Topologically sort the cabal build plan, and build the
  -- packages in sequence.
  = TopoSort

  -- | Asynchronously build all the packages, with each package
  -- waiting on its dependencies.
  | Async
     AsyncSem
       -- ^ The kind of semaphore to use to control concurrency.
  deriving stock Show

-- | What kind of semaphore to use in 'BuildEnv.Build.buildPlan'?
data AsyncSem
  -- | Don't use any semaphore (not recommended).
  = NoSem
  -- | Create a new 'Control.Concurrent.QSem.QSem' semaphore
  -- with the given number of tokens.
  | NewQSem !Word16
  -- | Create a new system semaphore with the given number of tokens,
  -- passing it to @ghc@ invocations.
  | NewJSem !Word16
  -- | Use an existing system semaphore, passing it to @ghc@ invocations.
  | ExistingJSem !String
  deriving stock Show

-- | A description of the kind of semaphore we are using to control concurrency.
semDescription :: AsyncSem -> Text
semDescription = \case
  NoSem     -> "no semaphore"
  NewQSem i -> "-j" <> Text.pack (show i)
  NewJSem i -> "--jsem " <> Text.pack (show i)
  ExistingJSem jsemName ->
    "--jsem " <> Text.pack jsemName

--------------------------------------------------------------------------------
-- Arguments

-- | A type synonym for command-line arguments.
type Args = [String]

-- | Arguments specific to a unit.
data UnitArgs =
  UnitArgs { configureArgs :: !Args
               -- ^ Arguments to @Setup configure@.
           , mbHaddockArgs :: !(Maybe Args)
               -- ^ Arguments to @Setup haddock@.
               -- @Nothing@ means: skip @Setup haddock@.
           , registerArgs  :: !Args
               -- ^ Arguments to @ghc-pkg register@.
           }
  deriving stock Show

--------------------------------------------------------------------------------
-- GHC & cabal-install

-- | Path to the @cabal-install@ executable.
data Cabal = Cabal { cabalPath       :: !( AbsolutePath File )
                   , globalCabalArgs :: !Args
                     -- ^ Arguments to pass to all @cabal@ invocations,
                     -- before any @cabal@ command.
                   }
  deriving stock Show

-- | Paths to the @ghc@ and @ghc-pkg@ executables.
data Compiler =
  Compiler { ghcPath    :: !( AbsolutePath File )
           , ghcPkgPath :: !( AbsolutePath File )
           }
  deriving stock Show

--------------------------------------------------------------------------------
-- Cabal Hackage index state

-- | Hackage index-state specification, e.g. 2022-12-25T00:00:00Z.
newtype IndexState = IndexState Text
  deriving newtype ( Show, Eq )

--------------------------------------------------------------------------------
-- Directory structure

-- | The directory structure relevant to preparing and carrying out
-- a build plan.
type Paths :: PathUsability -> Type
data Paths use
  = Paths
    { fetchDir   :: !( SymbolicPath Project ( Dir Fetch ) )
       -- ^ Input fetched sources directory.
    , buildPaths :: BuildPaths use
      -- ^ Output build directory structure.
      --
      -- NB: this will be bottom in the case that we are outputing
      -- a shell script that uses variables.
    }

deriving stock instance Show ( BuildPaths use ) => Show ( Paths use )

-- | The directory structure relevant to executing a build plan.
type BuildPaths :: PathUsability -> Type
data family BuildPaths use
data instance BuildPaths Raw
  = RawBuildPaths
    { rawDestDir :: !( SymbolicPath Project ( Dir Install ) )
      -- ^ Raw output build @destdir@ (might be relative).
    , rawPrefix  :: !( SymbolicPath Project ( Dir Prefix ) )
      -- ^ Raw output build @prefix@ (might be relative).
    }
  deriving stock Show
data instance BuildPaths ForPrep
  = BuildPathsForPrep
    { compilerForPrep :: !Compiler
      -- ^ Which @ghc@ and @ghc-pkg@ to use.
    , installDir      :: !( AbsolutePath ( Dir Install ) )
      -- ^ Output installation directory @destdir/prefix@ (absolute).
    }
  deriving stock Show
data instance BuildPaths ForBuild
  = BuildPaths
    { compiler   :: !Compiler
      -- ^ Which @ghc@ and @ghc-pkg@ to use.
    , prefix     :: !( AbsolutePath ( Dir Prefix ) )
      -- ^ Output build @prefix@ (absolute).
    , installDir :: !( AbsolutePath ( Dir Install ) )
      -- ^ Output installation directory @destdir/prefix@ (absolute).
    , logDir     :: !( AbsolutePath ( Dir Logs ) )
      -- ^ Directory in which to put logs.
    }
  deriving stock Show

-- | The appropriate stage at which to use a filepath.
data PathUsability
  -- | We have just parsed filepaths. They need to be canonicalised
  -- before they can be used.
  = Raw
  -- | The filepaths have been canonicalised.
  --
  -- They are now suitable for preparatory build instructions,
  -- but not for performing the build.
  | ForPrep
  -- | The paths are suitable for performing the build.
  | ForBuild

-- | Canonicalise raw 'Paths', computing the appropriate directory structure
-- for preparing and executing a build, respectively.
canonicalizePaths :: Compiler
                  -> BuildStrategy
                  -> SymbolicPath CWD ( Dir Project)
                  -> Paths Raw
                  -> IO ( Paths ForPrep, Paths ForBuild )
canonicalizePaths compiler buildStrat workDir
  ( Paths
    { fetchDir   = fetchDir
    , buildPaths = RawBuildPaths { rawPrefix, rawDestDir } } )
  = do
      prefix     <- makeAbsolute workDir rawPrefix
      installDir <- makeAbsolute workDir ( mkInstallDir rawDestDir prefix )


      logDir <- case buildStrat of
        Script  {} -> return $ mkAbsolutePath "${LOGDIR}" -- LOGDIR is defined by the script.
        Execute {} -> do
          -- Pick the logging directory based on the current time.
          time <- getCurrentTime
          makeAbsolute workDir
            ( mkSymbolicPath $ "logs" </> formatTime defaultTimeLocale "%0Y-%m-%d_%H-%M-%S" time )

      let forBuild = case buildStrat of
            Script { useVariables }
              | useVariables
              -> Paths { fetchDir   = mkSymbolicPath "${SOURCES}"
                       , buildPaths =
                         BuildPaths
                           { prefix     = mkAbsolutePath "${PREFIX}"
                           , installDir = mkAbsolutePath $ "${DESTDIR}" </> "${PREFIX}"
                           , logDir
                           , compiler =
                             Compiler { ghcPath    = mkAbsolutePath "${GHC}"
                                      , ghcPkgPath = mkAbsolutePath "${GHCPKG}" } } }
            _don'tUseVars ->
              Paths { fetchDir
                    , buildPaths =
                      BuildPaths { compiler, prefix, installDir, logDir } }
      return $
        ( Paths { fetchDir
                , buildPaths =
                  BuildPathsForPrep { compilerForPrep = compiler, installDir } }
        , forBuild )

mkInstallDir :: SymbolicPath Project ( Dir Install )
             -> AbsolutePath ( Dir Prefix )
             -> SymbolicPath Project ( Dir Install )
mkInstallDir destDir prefix =
  destDir </> mkRelativePath ( dropDrive $ getAbsolutePath prefix )
    -- We must use dropDrive here. Quoting from the documentation of (</>):
    --
    --   If the second path starts with a path separator or a drive letter,
    --   then (</>) returns the second path.
    --
    -- We don't want that, as we *do* want to concatenate both paths.

-- | How to handle deletion of temporary directories.
data TempDirPermanence
  = DeleteTempDirs
  | Don'tDeleteTempDirs
  deriving stock Show

--------------------------------------------------------------------------------
-- Verbosity

-- | Verbosity level for the @build-env@ package.
--
-- The default verbosity level is 'Normal' (1).
newtype Verbosity = Verbosity Int
  deriving newtype (Eq, Ord, Num)
  deriving stock   Show

-- | Get the flag corresponding to a verbosity, e.g. @-v2@.
verbosityFlag :: Verbosity -> String
verbosityFlag ( Verbosity i )
  | i <= 0
  = "-v0"
  | otherwise
  = "-v" <> show i

pattern Quiet, Normal, Verbose, Debug :: Verbosity
pattern Quiet   = Verbosity 0
pattern Normal  = Verbosity 1
pattern Verbose = Verbosity 2
pattern Debug   = Verbosity 3

quietMsg, normalMsg, verboseMsg, debugMsg :: Verbosity -> Text -> IO ()
quietMsg   v msg = when (v >= Quiet  ) $ putMsg msg
normalMsg  v msg = when (v >= Normal ) $ putMsg msg
verboseMsg v msg = when (v >= Verbose) $ putMsg msg
debugMsg   v msg = when (v >= Debug  ) $ putMsg msg

-- | Write the text to @stdout@, and flush.
putMsg :: Text -> IO ()
putMsg msg = do
  Text.putStrLn msg
  hFlush stdout

ghcVerbosity, ghcPkgVerbosity, cabalVerbosity, setupVerbosity
  :: Verbosity -> String
ghcVerbosity    = verbosityFlag . min maxGhcVerbosity    . subtract 1
ghcPkgVerbosity = verbosityFlag . min maxGhcPkgVerbosity . subtract 1
cabalVerbosity  = verbosityFlag . min maxCabalVerbosity  . subtract 1
setupVerbosity  = verbosityFlag . min maxSetupVerbosity  . subtract 1

maxGhcVerbosity, maxGhcPkgVerbosity, maxCabalVerbosity, maxSetupVerbosity
  :: Verbosity
maxGhcVerbosity    = Verbosity 3
maxGhcPkgVerbosity = Verbosity 2
maxCabalVerbosity  = Verbosity 3
maxSetupVerbosity  = maxCabalVerbosity

--------------------------------------------------------------------------------
-- Reporting progress.

-- | A counter to measure progress, as units are compiled.
data Counter =
  Counter
    { counterRef  :: !( IORef Word )
      -- ^ The running count.
    , counterMax :: !Word
      -- ^ The maximum that we're counting up to.
    }

--------------------------------------------------------------------------------
-- Posix/Windows style differences.

-- | Whether to use Posix or Windows style:
--
--  - for executables, @./prog@ vs @prog.exe@,
--  - for the path separator, @:@ vs @;@.
data Style
  = PosixStyle
  | WinStyle

-- | OS-dependent separator for the PATH environment variable.
pATHSeparator :: Style -> String
pATHSeparator PosixStyle = ":"
pATHSeparator WinStyle   = ";"

-- | The style associated with the OS the program is currently running on.
hostStyle :: Style
hostStyle =
#if defined(mingw32_HOST_OS)
  WinStyle
#else
  PosixStyle
#endif
