{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  BuildEnv.Options
-- Description :  Options for the command-line interface of @build-env@
--
-- This module declares datatypes which specify the structure of the options
-- expected by the command-line interface of @build-env@.
module BuildEnv.Options where

-- containers
import Data.Set
  ( Set )

-- build-env
import BuildEnv.CabalPlan
import BuildEnv.Config

--------------------------------------------------------------------------------

-- | The command-line options for the @build-env@ application.
data Opts = Opts { compiler   :: Compiler
                 , cabal      :: Cabal
                 , mode       :: Mode
                 , verbosity  :: Verbosity
                 , delTemp    :: TempDirPermanence
                 , workDir    :: FilePath
                 , indexState :: Maybe IndexState
                 }

-- | The mode in which to run the executable:
--
--   - compute a build plan,
--   - fetch sources,
--   - build and register packages.
data Mode
  -- | Find a build plan.
  = PlanMode
     { planModeInputs :: PlanInputs
     , planOutput     :: FilePath
       -- ^ Where to output the @plan.json@ file.
     }
  -- | Fetch sources from a build plan.
  | FetchMode
      FetchDescription -- ^ what to fetch
      NewOrExisting    -- ^ whether to create a new directory
                       -- or add to an existing one
  -- | Build and register packages from fetched sources.
  | BuildMode Build

-- | How to specify which packages/units to constraint/build.
data PackageData pkgs
  -- | Explicit description of packages/units.
  = Explicit pkgs
  -- | Parse package information from the given file.
  --
  -- The file contents will be interpreted as follows:
  --
  --   - pinned packages: this is a @cabal.config@ freeze file,
  --     which uses @cabal.project@ syntax. See 'readCabalDotConfig'.
  --
  --   - seed units: this is a list of seed units to build,
  --     with inline flags and constraints, and allow-newer stanzas.
  --     See 'parseSeedFile'.
  | FromFile FilePath
  deriving stock Show

-- | Inputs for the computation of a cabal plan.
data PlanInputs
  = PlanInputs
    { planUnits      :: PackageData UnitSpecs
      -- ^ Seed dependencies for the build plan.
    , planPins       :: Maybe (PackageData PkgSpecs)
      -- ^ Additional package constraints.
    , planAllowNewer :: AllowNewer
      -- ^ Allow-newer specification.
    }
  deriving stock Show

-- | Information about fetched sources: in which directory they belong,
-- and what build plan they correspond to.
data FetchDescription
  = FetchDescription
    { rawFetchDir    :: FilePath
      -- ^ Directory for fetched sources.
    , fetchInputPlan :: Plan
      -- ^ The build plan corresponding to the fetched sources.
    }
  deriving stock Show

-- | How to obtain a plan: either by computing it,
-- or by using an existing @plan.json@ plan.
data Plan
  -- | Compute a plan.
  = ComputePlan
      PlanInputs
        -- ^ Input needed to compute the plan.
      (Maybe FilePath)
        -- ^ Optional filepath at which to write out the computed plan.

  -- | Use an existing @plan.json@ by reading the given file.
  | UsePlan
    { planJSONPath :: FilePath }
  deriving stock Show

-- | At which point to start/continue a build.
data BuildStart
  -- | Fetch sources and start a new build.
  = Fetch NewOrExisting
  -- | Start a new build from prefetched sources.
  | Prefetched
  -- | Resume a previous build.
  | Resume
  deriving stock ( Show, Eq, Ord )

-- | Whether to create a new directory or use an existing directory.
data NewOrExisting
  -- | Create a new directory.
  = New
  -- | Update an existing directory.
  | Existing
  deriving stock ( Show, Eq, Ord )

-- | Information needed to perform a build.
data Build
  = Build
    { buildRawPaths   :: Paths Raw
      -- ^ The directories relevant for the build:
      --
      --  - fetched sources directory,
      --  - build output directory structure
    , buildStart      :: BuildStart
      -- ^ At which stage should the build start?
    , buildBuildPlan  :: Plan
      -- ^ The build plan to follow.
    , buildStrategy   :: BuildStrategy
      -- ^ How to perform the build (see 'BuildStrategy').
    , mbOnlyDepsOf    :: Maybe ( Set PkgName )
      -- ^ @Just pkgs@ <=> only build @pkgs@ (and their dependencies).
      --   @Nothing@ <=> build all units in the build plan.
    , eventLogDir     :: Maybe FilePath
      -- ^ Optional output directory in which to output GHC eventlogs
      -- for the build.
    , userUnitArgs    :: ConfiguredUnit -> UnitArgs
      -- ^ Extra per-unit arguments.
    }

