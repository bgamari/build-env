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

   -- * Passing arguments
  , Args, UnitArgs(..)

    -- * @ghc@ and @cabal-install@ executables
  , Compiler(..), Cabal(..)

    -- * Directory structure
  , Dirs(..), PathType(..), StagedPath
  , canonicalizeDirs, dirsForOutput

    -- ** Handling of temporary directories
  , TempDirPermanence(..)

    -- * Logging verbosity
  , Verbosity(.., Quiet, Normal, Verbose, Debug)
  , quietMsg, normalMsg, verboseMsg, debugMsg
  , ghcVerbosity, ghcPkgVerbosity, cabalVerbosity, setupVerbosity

    -- * OS specifics
  , Style(..), hostStyle
  , pATHSeparator

  ) where

-- base
import Control.Monad
  ( when )
import Data.Kind
  ( Type )
import Data.Word
  ( Word8 )

-- directory
import System.Directory
  ( canonicalizePath )

-- filepath
import System.FilePath
  ( (</>), dropDrive )

-- text
import Data.Text
  ( Text )
import qualified Data.Text.IO as Text
  ( putStrLn )

--------------------------------------------------------------------------------
-- Build strategy

-- | Build strategy for 'buildPlan'.
data BuildStrategy
  -- | Topologically sort the cabal build plan, and build the
  -- packages in sequence.
  = TopoSort
  -- | Asynchronously build all the packages, with each package
  -- waiting on its dependencies.
  | Async Word8
  -- | Output a build script that can be run later.
  | Script
    { scriptPath :: FilePath
      -- ^ Output path at which to write the build script.
    , useVariables :: Bool
      -- ^ Replace various values with variables, so that
      -- they can be set before running the build script.
      --
      -- Values:
      --
      --  - @GHC@ and @GHC-PKG@,
      --  - fetched sources directory @SOURCES@,
      --  - @PREFIX@ and @DESTDIR@.
    }
  deriving stock Show

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

-- | The directory structure relevant to a build. These are:
--
-- - the input fetched sources directory
-- - the build output directory structure: @destdir@ and @prefix@,
--   which determine @installdir@.
--
-- Meaning of the type parameter:
--
-- - 'Raw': filepaths can be relative.
-- - 'Canonicalised': the filepaths must be absolute.
-- - 'ForOutput': filepaths may be variables (when outputting a shell script).
type Dirs :: PathType -> Type
data Dirs pathTy =
  Dirs
    { fetchDir    :: FilePath
      -- ^ Input fetched sources directory (build input).
    , destDir     :: FilePath
      -- ^ Output build @destdir@.
    , prefix      :: FilePath
      -- ^ Output build @prefix@.
    , installDir  :: StagedPath pathTy
      -- ^ - 'Raw' stage: trivial value.
      --   - 'Canonicalised' stage: the output installation directory @destdir/prefix@.
    }

deriving stock instance Show (StagedPath pathTy) => Show (Dirs pathTy)

data PathType = Raw | Canonicalised | ForOutput

-- | We need to canonicalise paths before computing an installation directory.
--
-- This type family keeps track of this canonicalisation step.
type StagedPath :: PathType -> Type
type family StagedPath pathTy where
  StagedPath Raw           = ()
  StagedPath Canonicalised = FilePath
  StagedPath ForOutput     = FilePath

-- | Canonicalise 'Dirs', computing the appropriate
-- installation directory @destdir/prefix@.
canonicalizeDirs :: Dirs Raw -> IO (Dirs Canonicalised)
canonicalizeDirs ( Dirs { fetchDir = fetchDir0
                        , destDir  = destDir0
                        , prefix   = prefix0 } )
  = do
      fetchDir   <- canonicalizePath fetchDir0
      prefix     <- canonicalizePath prefix0
      destDir    <- canonicalizePath destDir0
      installDir <- canonicalizePath ( destDir0 </> dropDrive prefix )
        -- We must use dropDrive here. Quoting from the documentation of (</>):
        --
        --   If the second path starts with a path separator or a drive letter,
        --   then (</>) returns the second path.
        --
        -- We don't want that, as we *do* want to concatenate both paths.
      return $ Dirs { fetchDir, destDir, prefix, installDir }

-- | Compute the directory structure we should use for output.
--
-- Replaces everything with variables if the boolean is 'True'; otherwise
-- returns the input directory structure.
dirsForOutput :: Bool -- ^ use variables?
              -> Dirs Canonicalised
              -> Dirs ForOutput
dirsForOutput useVars ( Dirs { fetchDir, destDir, prefix, installDir })
  | useVars
  = Dirs { fetchDir   = "$SOURCES"
         , prefix     = "$PREFIX"
         , destDir    = "$DESTDIR"
         , installDir = "$DESTDIR/$PREFIX" }
  | otherwise
  = Dirs { fetchDir, destDir, prefix, installDir }

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
