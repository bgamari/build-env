module Options where

-- build-env
import CabalPlan
  ( PkgSpecs, AllowNewer )
import Config
  ( Compiler, Cabal, BuildStrategy )

--------------------------------------------------------------------------------

-- | The command-line options for the @build-env@ application.
data Opts = Opts { compiler  :: Compiler
                 , cabal     :: Cabal
                 , mode      :: Mode
                 , verbosity :: Verbosity
                 }
  deriving stock Show

-- | Verbosity level for the @build-env@ application.
newtype Verbosity = Verbosity Int
  deriving newtype (Eq, Ord)
  deriving stock   Show

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
  | FetchMode FetchDescription
  -- | Build and register packages from fetched sources.
  | BuildMode Build
  deriving stock Show

-- | Inputs to computing a cabal plan.
data PlanInputs
  = PlanInputs
    { planPins       :: PkgSpecs
    , planPkgs       :: PkgSpecs
    , planAllowNewer :: AllowNewer }
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
  = ComputePlan PlanInputs
  -- | Use an existing @plan.json@ by reading the given file.
  | UsePlan
    { planJSONPath :: FilePath }
  deriving stock Show

-- | Whether to fetch the sources or to use prefetched sources.
data Fetch
  -- | Fetch the sources.
  = Fetch
  -- | The sources have already been fetched.
  | Prefetched
  deriving stock Show

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
    , buildOutputDir  :: FilePath
      -- ^ The output directory for the build.
    }
  deriving stock Show
