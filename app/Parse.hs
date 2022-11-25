
{-# LANGUAGE ApplicativeDo #-}

module Parse ( options, runOptionsParser ) where

-- base
import Data.Bool ( bool )

-- containers
import qualified Data.Map.Strict as M
  ( empty, fromList )

-- optparse-applicative
import Options.Applicative

-- build-env
import CabalPlan
import Config
import Options

--------------------------------------------------------------------------------

-- | Run the command-line options parser.
runOptionsParser :: IO Opts
runOptionsParser =
  customExecParser ( prefs showHelpOnEmpty ) $
    info (helper <*> options)
      (  fullDesc
      <> header "build-env - compute, fetch and build cabal build plans" )

-- | The command-line options parser for the 'build-env' executable.
options :: Parser Opts
options = do
  mode      <- optMode
  compiler  <- optCompiler
  cabal     <- optCabal
  verbosity <- optVerbosity
  return $ Opts { compiler, cabal, mode, verbosity }

-- | Parse @ghc@ and @ghc-pkg@ paths.
optCompiler :: Parser Compiler
optCompiler =
    Compiler
      <$> option str (long "ghc" <> value "ghc" <> help "'ghc' executable path" <> metavar "GHC")
      <*> option str (long "ghc-pkg" <> value "ghc-pkg" <> help "'ghc-pkg' executable path" <> metavar "GHC-PKG")

-- | Parse @cabal@ path.
optCabal :: Parser Cabal
optCabal =
  Cabal <$> option str (long "cabal" <> value "cabal" <> help "'cabal' executable path" <> metavar "CABAL")

-- | Parse verbosity.
optVerbosity :: Parser Verbosity
optVerbosity =
  Verbosity <$>
    flag 0 1 ( long "verbose" <> short 'v' <> help "Enable verbose mode" )

-- | Parse the mode in which to run the application: plan, fetch, build.
optMode :: Parser Mode
optMode =
  hsubparser . mconcat $
    [ command "plan"  $
        info ( PlanMode  <$> planInputs "Build plan seed packages" <*> optOutput )
        ( progDesc "Compute a build plan" )
    , command "fetch" $
        info ( FetchMode <$> fetchDescription "Seed packages to fetch" )
        ( fullDesc <> progDesc "Fetch package sources" )
    , command "build" $
        info ( BuildMode <$> build )
        ( fullDesc <> progDesc "Build and register packages" )
    ]
  where
    optOutput :: Parser FilePath
    optOutput =
      option str ( short 'o' <> long "output" <> help "Output 'plan.json' filepath" )

-- | A 'String' that's only used to set a parser description.
type OptionDescString = String

-- | Obtain a collection of seed packages.
--
-- Might be for computing a build plan, fetching sources, or building packages.
planInputs :: OptionDescString -> Parser PlanInputs
planInputs pkgsParserDesc = do

  let
    -- TODO: not allowing pinned packages or allow-newer yet.
    -- It would be good to allow these to be read from files
    -- instead of passed in the command line.
    planPins :: PkgSpecs
    planPins = M.empty
    planAllowNewer :: AllowNewer
    planAllowNewer = AllowNewer []
  planPkgs <- pkgs

  return $ PlanInputs { planPins, planPkgs, planAllowNewer }

  where
    pkgs :: Parser PkgSpecs
    pkgs = M.fromList <$> many (argument pkgSpec (metavar "PKG1 PKG2 ..." <> help pkgsParserDesc))

    pkgSpec :: ReadM (PkgName, PkgSpec)
    pkgSpec = (,) <$> (PkgName <$> str) <*> (PkgSpec Nothing <$> pure mempty)

-- | Parse how we will obtain a build plan: by computing it, or by reading
-- from a @plan.json@ on disk?
plan :: OptionDescString -> Parser Plan
plan pkgsParserDesc = do
  inputs     <- planInputs pkgsParserDesc
  mbPlanPath <- optPlanPath
  return $
    case mbPlanPath of
      -- Passing the path to a plan.json overrides everything else.
      -- TODO: emit a warning/error instead of silently discarding.
      Just planJSONPath -> UsePlan { planJSONPath }
      Nothing           -> ComputePlan inputs

  where
    optPlanPath :: Parser (Maybe FilePath)
    optPlanPath =
      option (fmap Just str)
        (short 'p' <> long "plan" <> value Nothing <> help "Path of a plan.json to use (overrides PKGs)" )

-- | Parse information about fetched sources: in which directory they belong,
-- and what build plan they correspond to.
fetchDescription :: OptionDescString -> Parser FetchDescription
fetchDescription pkgsParserDesc = do
  fetchInputPlan <- plan pkgsParserDesc
  fetchDir       <- optFetchDir
  return $ FetchDescription { fetchDir, fetchInputPlan }

  where
    optFetchDir :: Parser FilePath
    optFetchDir =
      option str ( short 'f' <> long "fetch-dir" <> help "Directory for fetched sources" )

-- | Parse the options for the @build@ command.
build :: Parser Build
build = do

  buildFetchDescr <- fetchDescription "Packages to build"
  buildFetch      <- optFetch
  buildStrategy   <- optStrategy
  buildOutputDir  <- optOutputDir

  return $ Build { buildFetch, buildFetchDescr
                 , buildStrategy, buildOutputDir }

  where

    optStrategy :: Parser BuildStrategy
    optStrategy =
      bool Async TopoSort <$>
        switch ( long "no-async" <> help "Disable asynchronous package building (useful for debugging)" )

    optFetch :: Parser Fetch
    optFetch =
      bool Fetch Prefetched <$>
        switch ( long "prefetched" <> help "Use prefetched sources instead of fetching from Hackage")

    optOutputDir :: Parser FilePath
    optOutputDir =
      option str ( short 'o' <> long "output-dir" <> help "Output directory" )
