module Config where

-- | Path to the @cabal@ executable.
data Cabal = Cabal { cabalPath :: FilePath }
  deriving stock Show

-- | Paths to the @ghc@ and @ghc-pkg@ executables.
data Compiler =
  Compiler { ghcPath    :: FilePath
           , ghcPkgPath :: FilePath
           }
  deriving stock Show

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
