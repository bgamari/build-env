{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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
    BuildStrategy(..)
  , AsyncSem(..)
  , semDescription

   -- * Passing arguments
  , Args, UnitArgs(..)

    -- * @ghc@ and @cabal-install@ executables
  , Compiler(..), Cabal(..)

    -- * Directory structure
  , DestDir(..), PathType(..), InstallDir
  , canonicalizeDestDir

    -- ** Handling of temporary directories
  , TempDirPermanence(..)

    -- * Logging verbosity
  , Verbosity(.., Quiet, Normal, Verbose, Debug)
  , quietMsg, normalMsg, verboseMsg, debugMsg
  , ghcVerbosity, ghcPkgVerbosity, cabalVerbosity, setupVerbosity

    -- * OS specifics
  , Style(..), hostStyle
  , runCwdExe, pATHSeparator

  ) where

-- base
import Control.Monad
  ( when )
import Data.Kind
  ( Type )
import Data.Word
  ( Word16 )

-- directory
import System.Directory
  ( canonicalizePath )

-- filepath
import System.FilePath
  ( (</>), (<.>), dropDrive )

#if BUILDENV_ENABLE_JSEM
-- semaphore-compat
import qualified System.Semaphore as System
  ( SemaphoreName(..) )
#endif

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( pack )
import qualified Data.Text.IO as Text
  ( putStrLn )

--------------------------------------------------------------------------------
-- Build strategy

-- | Build strategy for 'BuildEnv.Build.buildPlan'.
data BuildStrategy
  -- | Topologically sort the cabal build plan, and build the
  -- packages in sequence.
  = TopoSort

  -- | Asynchronously build all the packages, with each package
  -- waiting on its dependencies.
  | Async
      AsyncSem -- ^ The kind of semaphore to use to control concurrency.

  -- | Output a build script that can be run later.
  | Script FilePath
  deriving stock Show

-- | What kind of semaphore to use in 'BuildEnv.Build.buildPlan'?
--
-- NB: this datatype depends on whether the @jsem@ flag
-- was enabled when building the @build-env@ package.
data AsyncSem
  -- | Don't use any semaphore (not recommended).
  = NoSem
  -- | Create a new 'Control.Concurrent.QSem.QSem' semaphore
  -- with the given number of tokens.
  | NewQSem Word16
#if BUILDENV_ENABLE_JSEM
  -- | __@jsem@ only:__ create a new system semaphore with the given number
  -- of tokens, passing it to @ghc@ invocations.
  | NewJSem Word16
  -- | __@jsem@ only:__ use an existing system semaphore,
  -- passing it to @ghc@ invocations.
  | ExistingJSem System.SemaphoreName
#endif
  deriving stock Show

-- | A description of the kind of semaphore we are using to control concurrency.
semDescription :: AsyncSem -> Text
semDescription = \case
  NoSem     -> "no semaphore"
  NewQSem i -> "-j" <> Text.pack (show i)
#if BUILDENV_ENABLE_JSEM
  NewJSem i -> "--jsem " <> Text.pack (show i)
  ExistingJSem ( System.SemaphoreName jsemName ) ->
    "--jsem " <> Text.pack jsemName
#endif

--------------------------------------------------------------------------------
-- Arguments

-- | A type synonym for command-line arguments.
type Args = [String]

-- | Arguments specific to a unit.
data UnitArgs =
  UnitArgs { configureArgs :: Args
               -- ^ Arguments to @Setup configure@.
           , mbHaddockArgs :: Maybe Args
               -- ^ Arguments to @Setup haddock@.
               -- @Nothing@ means: skip @Setup haddock@.
           , registerArgs  :: Args
               -- ^ Arguments to @ghc-pkg register@.
           }
  deriving stock Show

--------------------------------------------------------------------------------
-- GHC & cabal-install

-- | Path to the @cabal-install@ executable.
data Cabal = Cabal { cabalPath :: FilePath
                   , globalCabalArgs :: Args
                     -- ^ Arguments to pass to all @cabal@ invocations,
                     -- before any @cabal@ command.
                   }
  deriving stock Show

-- | Paths to the @ghc@ and @ghc-pkg@ executables.
data Compiler =
  Compiler { ghcPath    :: FilePath
           , ghcPkgPath :: FilePath
           }
  deriving stock Show

--------------------------------------------------------------------------------
-- Directory structure

-- | The directory structure relevant to installation: @dest-dir@ and @prefix@.
--
-- If the type parameter is 'Raw', filepaths can be relative.
-- If it is 'Canonicalised', the filepaths must be absolute.
--
-- See 'installDir'.
type DestDir :: PathType -> Type
data DestDir pathTy =
  DestDir
    { destDir     :: FilePath
      -- ^ Build @destdir@.
    , prefix      :: FilePath
      -- ^ The build @prefix@.
    , installDir  :: InstallDir pathTy
      -- ^ The installation directory @dest-dir/prefix@.
    }

deriving stock instance Show (InstallDir pathTy) => Show (DestDir pathTy)

data PathType = Raw | Canonicalised

-- | We need to canonicalise paths before computing an installation directory.
--
-- This type family keeps track of whether we have done so or not.
type InstallDir :: PathType -> Type
type family InstallDir pathTy where
  InstallDir Raw           = ()
  InstallDir Canonicalised = FilePath

-- | Canonicalise a 'DestDir', computing the appropriate
-- installation directory @dest-dir/prefix@.
canonicalizeDestDir :: DestDir Raw -> IO (DestDir Canonicalised)
canonicalizeDestDir ( DestDir { destDir = destDir0, prefix = prefix0 } ) = do
  prefix     <- canonicalizePath prefix0
  destDir    <- canonicalizePath destDir0
  installDir <- canonicalizePath ( destDir0 </> dropDrive prefix )
    -- We must use dropDrive here. Quoting from the documentation of (</>):
    --
    --   If the second path starts with a path separator or a drive letter,
    --   then (</>) returns the second path.
    --
    -- We don't want that, as we *do* want to concatenate both paths.
  return $ DestDir { destDir, prefix, installDir }

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
  deriving newtype (Eq, Ord)
  deriving stock   Show

pattern Quiet, Normal, Verbose, Debug :: Verbosity
pattern Quiet   = Verbosity 0
pattern Normal  = Verbosity 1
pattern Verbose = Verbosity 2
pattern Debug   = Verbosity 3

quietMsg, normalMsg, verboseMsg, debugMsg :: Verbosity -> Text -> IO ()
quietMsg   v msg = when (v >= Quiet  ) $ Text.putStrLn msg
normalMsg  v msg = when (v >= Normal ) $ Text.putStrLn msg
verboseMsg v msg = when (v >= Verbose) $ Text.putStrLn msg
debugMsg   v msg = when (v >= Debug  ) $ Text.putStrLn msg

ghcVerbosity, ghcPkgVerbosity, cabalVerbosity, setupVerbosity
  :: Verbosity -> String
ghcVerbosity (Verbosity i)
  | i <= 1
  = "-v0"
  | otherwise
  = "-v1"
ghcPkgVerbosity = ghcVerbosity
setupVerbosity  = ghcVerbosity
cabalVerbosity (Verbosity i)
  | i <= 1
  = "-v0"
cabalVerbosity (Verbosity 2) = "-v1"
cabalVerbosity (Verbosity 3) = "-v2"
cabalVerbosity (Verbosity _) = "-v3"

--------------------------------------------------------------------------------
-- Posix/Windows style differences.

-- | Whether to use Posix or Windows style:
--
--  - for executables, @./prog@ vs @prog.exe@,
--  - for the path separator, @:@ vs @;@.
data Style
  = PosixStyle
  | WinStyle

-- | Command to run an executable in the current working directory.
runCwdExe :: Style -> FilePath -> FilePath
runCwdExe PosixStyle exe = "./" <> exe
runCwdExe WinStyle   exe = exe <.> "exe"

-- | OS-dependent separator for the PATH environment variable.
pATHSeparator :: Style -> String
pATHSeparator PosixStyle = ":"
pATHSeparator WinStyle   = ";"

-- | The style for the OS the program is currently running on.
hostStyle :: Style
hostStyle =
#if defined(mingw32_HOST_OS)
  WinStyle
#else
  PosixStyle
#endif
