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
  , AsyncSem(..), semDescription

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

-- directory
import System.Directory
  ( canonicalizePath )

-- filepath
import System.FilePath
  ( (</>), dropDrive )

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
  -- | Execute the build plan in-place.
  = Execute RunStrategy
  -- | Output a build script that can be run later.
  | Script
    { scriptPath   :: !FilePath
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
--
-- NB: this datatype depends on whether the @jsem@ flag
-- was enabled when building the @build-env@ package.
data AsyncSem
  -- | Don't use any semaphore (not recommended).
  = NoSem
  -- | Create a new 'Control.Concurrent.QSem.QSem' semaphore
  -- with the given number of tokens.
  | NewQSem !Word16
  deriving stock Show

-- | A description of the kind of semaphore we are using to control concurrency.
semDescription :: AsyncSem -> Text
semDescription = \case
  NoSem     -> "no semaphore"
  NewQSem i -> "-j" <> Text.pack (show i)

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
    , buildPaths :: BuildPaths use
      -- ^ Output build directory structure.
      --
      -- NB: this will be bottom in the case that we are outputing
      -- a shell script that uses variables.
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
  = BuildPathsForPrep
    { compilerForPrep :: !Compiler
      -- ^ Which @ghc@ and @ghc-pkg@ to use
    }
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
-- for preparing and executing a build, respectively.
canonicalizePaths :: Compiler
                  -> BuildStrategy
                  -> Paths Raw
                  -> IO ( Paths ForPrep, Paths ForBuild )
canonicalizePaths compiler buildStrat
  ( Paths
    { fetchDir   = fetchDir0
    , buildPaths = rawBuildPaths } )
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
          _don'tUseVars
            | RawBuildPaths { rawPrefix, rawDestDir } <- rawBuildPaths
            -- If we use variables, 'rawBuildPaths' might be undefined,
            -- so only force it after checking whether we are using variables.
            -> do
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
        ( Paths { fetchDir, buildPaths = BuildPathsForPrep { compilerForPrep = compiler } }, forBuild )

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
cabalVerbosity (Verbosity i)
  | i <= 1
  = "-v0"
cabalVerbosity (Verbosity 2) = "-v1"
cabalVerbosity (Verbosity 3) = "-v2"
cabalVerbosity (Verbosity _) = "-v3"
ghcVerbosity    = cabalVerbosity
ghcPkgVerbosity v@(Verbosity i)
  | i >= 3
  = "-v2"
  | otherwise
  = cabalVerbosity v
setupVerbosity  = cabalVerbosity

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
