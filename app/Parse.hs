
{-# LANGUAGE ApplicativeDo #-}

module Parse ( options ) where

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

options :: Parser Opts
options = do
  mode      <- optMode
  compiler  <- optCompiler
  cabal     <- optCabal
  verbosity <- optVerbosity
  return $ Opts { compiler, cabal, mode, verbosity }


optCompiler :: Parser Compiler
optCompiler =
    Compiler
      <$> option str (long "ghc" <> value "ghc" <> help "'ghc' executable path" <> metavar "GHC")
      <*> option str (long "ghc-pkg" <> value "ghc-pkg" <> help "'ghc-pkg' executable path" <> metavar "GHC-PKG")

optCabal :: Parser Cabal
optCabal =
  Cabal <$> option str (long "cabal" <> value "cabal" <> help "'cabal' executable path" <> metavar "CABAL")

optVerbosity :: Parser Verbosity
optVerbosity =
  Verbosity <$>
    flag 0 1 ( long "verbose" <> short 'v' <> help "Enable verbose mode" )

optMode :: Parser Mode
optMode =
  hsubparser . mconcat $
    [ command "plan"  $
        info ( PlanMode  <$> planInputs <*> optOutput )
        ( progDesc "Compute a build plan" )
    , command "fetch" $
        info ( FetchMode <$> fetchInputs )
        ( progDesc "Fetch package sources" )
    , command "build" $
        info ( BuildMode <$> build )
        ( progDesc "Build and register packages" )
    ]
  where
    optOutput :: Parser FilePath
    optOutput =
      option str ( short 'o' <> long "output" <> help "output filepath for plan.json" )


planInputs :: Parser PlanInputs
planInputs = do

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
    pkgs = M.fromList <$> many (argument pkgSpec (help "package" <> metavar "PKG"))

    pkgSpec :: ReadM (PkgName, PkgSpec)
    pkgSpec = (,) <$> (PkgName <$> str) <*> (PkgSpec Nothing <$> pure mempty)

plan :: Parser Plan
plan = do
  mbPlanPath <- optPlanPath
  inputs     <- planInputs
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
        (short 'p' <> long "plan" <> value Nothing <> help "Path of a plan.json to use" )

fetchInputs :: Parser FetchInputs
fetchInputs = do
  fetchDir       <- optFetchDir
  fetchInputPlan <- plan
  return $ FetchInputs { fetchDir, fetchInputPlan }

  where
    optFetchDir :: Parser FilePath
    optFetchDir =
      option str ( short 'f' <> long "fetch-dir" <> help "Fetch directory" )

build :: Parser Build
build = do

  buildFetch       <- optFetch
  buildFetchInputs <- fetchInputs
  buildStrategy    <- optStrategy
  buildOutputDir   <- optOutputDir

  return $ Build { buildFetch, buildFetchInputs
                 , buildStrategy, buildOutputDir }

  where

    optStrategy :: Parser BuildStrategy
    optStrategy =
      bool Async TopoSort <$>
        switch ( long "no-async" <> help "Disable asynchronous package building\n  (useful for debugging)" )

    optFetch :: Parser Fetch
    optFetch =
      bool Prefetched Fetch <$>
        switch ( long "fetch" <> help "Fetch packages from Hackage" )

    optOutputDir :: Parser FilePath
    optOutputDir =
      option str ( short 'o' <> long "output-dir" <> help "Output directory" )
