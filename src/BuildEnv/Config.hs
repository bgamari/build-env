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
    BuildStrategy(..), RunStrategy(..)

   -- * Passing arguments
  , Args, UnitArgs(..)

    -- * @ghc@ and @cabal-install@ executables
  , Compiler(..), Cabal(..)

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
  -- | Execute the build plan right away, in 'IO'.
  = Execute RunStrategy
  -- | Output a build script that can be run later.
  | Script
    { scriptPath   :: !FilePath
      -- ^ Output path at which to write the build script.
    , useVariables :: !Bool
      -- ^ Should the output shell script use variables, or baked in paths?
      --
      -- Variables are:
      --
      --  - @GHC@ and @GHC-PKG@,
      --  - fetched sources directory @SOURCES@,
      --  - @PREFIX@ and @DESTDIR@.
    }
  deriving stock Show

-- | How to execute a build plan.
data RunStrategy
  -- | Topologically sort the cabal build plan, and build the
  -- packages in sequence.
  = TopoSort
  -- | Asynchronously build all the packages, with each package
  -- waiting on its dependencies.
  | Async !Word8
  deriving stock Show

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
data Cabal = Cabal { cabalPath       :: !FilePath
                   , globalCabalArgs :: !Args
                     -- ^ Arguments to pass to all @cabal@ invocations,
                     -- before any @cabal@ command.
                   }
  deriving stock Show

-- | Paths to the @ghc@ and @ghc-pkg@ executables.
data Compiler =
  Compiler { ghcPath    :: !FilePath
           , ghcPkgPath :: !FilePath
           }
  deriving stock Show

--------------------------------------------------------------------------------
-- Directory structure

-- | The directory structure relevant to preparing and carrying out
-- a build plan.
type Paths :: PathUsability -> Type
data Paths use
  = Paths
    { fetchDir   :: !FilePath
       -- ^ Input fetched sources directory.
    , buildPaths :: !(BuildPaths use)
      -- ^ Output build directory structure.
    }

-- | The directory structure relevant to executing a build plan.
type BuildPaths :: PathUsability -> Type
data family BuildPaths use
data instance BuildPaths Raw
  = RawBuildPaths
    { rawDestDir :: !FilePath
      -- ^ Raw output build @destdir@ (might be relative).
    , rawPrefix  :: !FilePath
      -- ^ Raw output build @prefix@ (might be relative).
    }
data instance BuildPaths ForPrep
  = NoBuildPathsForPrep
    -- ^ Placeholder to ensure that no build paths are used
    -- during the build preparation stage.
data instance BuildPaths ForBuild
  = BuildPaths
    { compiler   :: !Compiler
      -- ^ Which @ghc@ and @ghc-pkg@ to use.
    , destDir    :: !FilePath
      -- ^ Output build @destdir@ (absolute).
    , prefix     :: !FilePath
      -- ^ Output build @prefix@ (absolute).
    , installDir :: !FilePath
      -- ^ Output installation directory @destdir/prefix@ (absolute).
    }

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
-- for preparing a build and executing a build.
canonicalizePaths :: Compiler
                  -> BuildStrategy
                  -> Paths Raw
                  -> IO ( Paths ForPrep, Paths ForBuild )
canonicalizePaths compiler buildStrat
  ( Paths
    { fetchDir   = fetchDir0
    , buildPaths = RawBuildPaths { rawPrefix, rawDestDir } } )
  = do
      fetchDir <- canonicalizePath fetchDir0
      forBuild <-
        case buildStrat of
          Script { useVariables }
            | useVariables
            -> return $
                Paths { fetchDir   = "${SOURCES}"
                      , buildPaths =
                        BuildPaths
                          { prefix     = "${PREFIX}"
                          , destDir    = "${DESTDIR}"
                          , installDir = "${DESTDIR}" </> "${PREFIX}"
                          , compiler =
                            Compiler { ghcPath = "${GHC}"
                                     , ghcPkgPath = "${GHCPKG}" } } }
          _don'tUseVars -> do
            prefix     <- canonicalizePath rawPrefix
            destDir    <- canonicalizePath rawDestDir
            installDir <- canonicalizePath ( rawDestDir </> dropDrive prefix )
              -- We must use dropDrive here. Quoting from the documentation of (</>):
              --
              --   If the second path starts with a path separator or a drive letter,
              --   then (</>) returns the second path.
              --
              -- We don't want that, as we *do* want to concatenate both paths.
            return $ Paths { fetchDir
                           , buildPaths =
                             BuildPaths { compiler, destDir, prefix, installDir } }
      return $
        ( Paths { fetchDir, buildPaths = NoBuildPathsForPrep }, forBuild )

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
