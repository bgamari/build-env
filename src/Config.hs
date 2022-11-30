-- |
-- Module      :  Config
-- Description :  Configuration options for @build-env@.
module Config where

-- base
import Control.Monad
  ( when )

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
