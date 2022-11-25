module Config where

data Cabal = Cabal { cabalPath :: FilePath }
  deriving stock Show

data Compiler =
  Compiler { ghcPath    :: FilePath
           , ghcPkgPath :: FilePath
           }
  deriving stock Show

data BuildStrategy
  = TopoSort
  | Async
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
