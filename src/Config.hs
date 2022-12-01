{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Config
-- Description :  Configuration options for @build-env@.
module Config where

-- base
import Control.Monad
  ( when )
import Data.Kind
  ( Type )

-- directory
import System.Directory
  ( canonicalizePath )

-- filepath
import System.FilePath
  ( (</>), dropDrive )

--------------------------------------------------------------------------------

-- | Path to the @cabal@ executable.
data Cabal = Cabal { cabalPath :: FilePath }
  deriving stock Show

-- | Paths to the @ghc@ and @ghc-pkg@ executables.
data Compiler =
  Compiler { ghcPath    :: FilePath
           , ghcPkgPath :: FilePath
           }
  deriving stock Show

-- | A type synonym for command-line arguments.
type Args = [String]

-- | Verbosity level for the @build-env@ package.
--
-- The default verbosity level is 1.
newtype Verbosity = Verbosity Int
  deriving newtype (Eq, Ord)
  deriving stock   Show

normalMsg, verboseMsg, debugMsg :: Verbosity -> String -> IO ()
normalMsg  v msg = when (v >= Verbosity 1) $ putStrLn msg
verboseMsg v msg = when (v >= Verbosity 2) $ putStrLn msg
debugMsg   v msg = when (v >= Verbosity 3) $ putStrLn msg

cabalVerbosity :: Verbosity -> String
cabalVerbosity (Verbosity i)
  | i <= 1
  = "-v0"
cabalVerbosity (Verbosity 2) = "-v1"
cabalVerbosity (Verbosity 3) = "-v2"
cabalVerbosity (Verbosity _) = "-v3"

-- | Build strategy for 'buildPlan'.
data BuildStrategy
  -- | Topologically sort the cabal build plan, and build the
  -- packages in sequence.
  = TopoSort
  -- | Asynchronously build all the packages, which each package
  -- waiting on its dependencies.
  | Async
  -- | Combination of 'TopoSort' and 'Async'.
  | TopoSortAsync
  deriving stock Show

doTopoSort :: BuildStrategy -> Bool
doTopoSort TopoSort      = True
doTopoSort Async         = False
doTopoSort TopoSortAsync = True

doAsync :: BuildStrategy -> Bool
doAsync TopoSort      = False
doAsync Async         = True
doAsync TopoSortAsync = True

-- | How to handle deletion of temporary directories.
data TempDirPermanence
  = DeleteTempDirs
  | Don'tDeleteTempDirs
  deriving stock Show

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
      -- ^ The build prefix.
    , installDir  :: InstallDir pathTy
      -- ^ The installation directory @<dest-dir>/<prefix>@.
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
-- installation directory @<dest-dir>/<prefix>@.
canonicalizeDestDir :: DestDir Raw -> IO (DestDir Canonicalised)
canonicalizeDestDir ( DestDir { destDir = destDir0, prefix = prefix0 }) = do
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
