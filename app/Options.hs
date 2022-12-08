{-# LANGUAGE DataKinds #-}

module Options where

-- build-env
import CabalPlan
import Config
import Target
  ( TargetArgs )

--------------------------------------------------------------------------------

-- | The command-line options for the @build-env@ application.
data Opts = Opts { compiler  :: Compiler
                 , cabal     :: Cabal
                 , mode      :: Mode
                 , verbosity :: Verbosity
                 , delTemp   :: TempDirPermanence
                 }
  deriving stock Show

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
  deriving stock Show

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
    { fetchDir       :: FilePath
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
        -- ^ input needed to compute the plan
      (Maybe FilePath)
        -- ^ optional filepath at which to write out the computed plan

  -- | Use an existing @plan.json@ by reading the given file.
  | UsePlan
    { planJSONPath :: FilePath }
  deriving stock Show

-- | Whether to fetch the sources or to use prefetched sources.
data Fetch
  -- | Fetch the sources.
  = Fetch NewOrExisting
  -- | The sources have already been fetched.
  | Prefetched
  deriving stock ( Show, Eq )

-- | Whether to create a new directory or use an existing directory.
data NewOrExisting
  -- | Create a new directory.
  = New
  -- | Update an existing directory.
  | Existing
  deriving stock ( Show, Eq )

-- | Information needed to perform a build.
data Build
  = Build
    { buildFetch      :: Fetch
      -- ^ How to obtain the fetched sources,
      -- including the build plan.
    , buildFetchDescr :: FetchDescription
      -- ^ Where the fetch sources are located,
      -- and the build plan they correspond to.
    , buildStrategy   :: BuildStrategy
      -- ^ How to perform the build (see 'BuildStrategy').
    , buildDestDir    :: DestDir Raw
      -- ^ The output directory for the build.
    , configureArgs   :: TargetArgs
      -- ^ Arguments to pass to the @setup configure@ script.
    , ghcPkgArgs      :: Args
      -- ^ Arguments to pass to @ghc-pkg register@.
    }
  deriving stock Show
