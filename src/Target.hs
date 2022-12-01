
-- |
-- Module      :  Target
-- Description :  Description of build targets
--
-- Description of possible targets to configure or build.
--
-- See 'Target'.
module Target where

-- containers
import Data.Map.Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( findWithDefault )

-- build-env
import Config
  ( Args )
import CabalPlan
  ( ComponentName, PkgName )

--------------------------------------------------------------------------------

-- | Arguments to pass for a specific target.
newtype TargetArgs
  = TargetArgs
  { targetArgs :: Map Target Args }
  deriving stock Show

-- | Targets:
--
--  - all packages,
--  - a specific package,
--  - a specific component of a specific package.
data Target
  -- | All packages.
  = AllPkgs
  -- | A specific package.
  | ThisPkg PkgName PkgTarget
  deriving stock ( Eq, Ord, Show )

-- | A target within a package.
data PkgTarget
  -- | All components of the package.
  = AllComponents
  -- | A specific component of the package.
  | ThisComponent ComponentName
  deriving stock ( Eq, Ord, Show )

-- | Retrieve the suitable arguments for a given component.
--
-- This includes arguments specific to this component as well as arguments
-- passed for all targets.
--
-- The package/component-specific arguments are passed after the more general
-- arguments, which allows overriding.
lookupTargetArgs :: TargetArgs -> PkgName -> ComponentName -> Args
lookupTargetArgs ( TargetArgs allArgs ) pkg comp  =
  mconcat [ Map.findWithDefault [] AllPkgs allArgs
          , Map.findWithDefault [] (ThisPkg pkg AllComponents) allArgs
          , Map.findWithDefault [] (ThisPkg pkg (ThisComponent comp)) allArgs ]
