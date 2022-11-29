
{-# LANGUAGE ApplicativeDo #-}

module Parse ( options, runOptionsParser ) where

-- base
import Data.Bool
  ( bool )

-- containers
import qualified Data.Map.Strict as M
  ( fromList )

-- optparse-applicative
import Options.Applicative

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( pack )

-- build-env
import CabalPlan
import Config
import Options

--------------------------------------------------------------------------------

-- | Run the command-line options parser.
runOptionsParser :: IO Opts
runOptionsParser =
  customExecParser ( prefs showHelpOnEmpty ) $
    info ( helper <*> options )
      (  fullDesc
      <> header "build-env - compute, fetch and build cabal build plans" )

-- | The command-line options parser for the 'build-env' executable.
options :: Parser Opts
options = do
  mode      <- optMode
  compiler  <- optCompiler
  cabal     <- optCabal
  verbosity <- optVerbosity
  delTemp   <- optTempDirPermanence
  return $ Opts { compiler, cabal, mode, verbosity, delTemp }

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
    option auto
      (  long "verbosity"
      <> short 'v'
      <> help "Verbosity"
      <> metavar "INT"
      <> value 1 )

-- | Parse whether to delete temporary directories.
optTempDirPermanence :: Parser TempDirPermanence
optTempDirPermanence =
    bool DeleteTempDirs Don'tDeleteTempDirs <$>
      switch (  long "preserve-tmp"
             <> help "Preserve temporary build directories (useful for debugging)" )

-- | Parse the mode in which to run the application: plan, fetch, build.
optMode :: Parser Mode
optMode =
  hsubparser . mconcat $
    [ command "plan"  $
        info ( PlanMode  <$> planInputs Planning <*> optOutput )
        ( progDesc "Compute a build plan from a collection of seeds" )
    , command "fetch" $
        info ( FetchMode <$> fetchDescription Fetching )
        ( fullDesc <> progDesc "Fetch package sources" )
    , command "build" $
        info ( BuildMode <$> build )
        ( fullDesc <> progDesc "Build and register packages" )
    ]
  where
    optOutput :: Parser FilePath
    optOutput =
      option str ( short 'p' <> long "plan" <> help "Output 'plan.json' file" <> metavar "OUTFILE" )


-- | Description of which mode we are in.
--
-- Used to generate help-text that is specific to a certain mode.
data ModeDescription
  = Planning
  | Fetching
  | Building
  deriving stock Show

modeDescription :: ModeDescription -> String
modeDescription modeDesc =
  case modeDesc of
    Planning -> "Build plan"
    Fetching -> "Fetch plan"
    Building -> "Build"

-- | Obtain a collection of seed packages.
--
-- Might be for computing a build plan, fetching sources, or building packages.
planInputs :: ModeDescription -> Parser PlanInputs
planInputs modeDesc = do

  planPkgs <- dependencies modeDesc
  planPins <- optional (freeze modeDesc)
  planAllowNewer <- allowNewer

  return $ PlanInputs { planPins, planPkgs, planAllowNewer }

-- | Parse a list of pinned packages from a 'cabal.config' freeze file.
freeze :: ModeDescription -> Parser PackageData
freeze modeDesc = FromFile <$> freezeFile
  where
    freezeFile :: Parser FilePath
    freezeFile = option str ( long "freeze" <> help helpStr <> metavar "INFILE" )

    helpStr :: String
    helpStr = modeDescription modeDesc <> " 'cabal.config' freeze file"

-- | Parse @allow-newer@ options.
allowNewer :: Parser AllowNewer
allowNewer =
  option readAllowNewer ( long "allow-newer" <> help "Allow-newer specification"
                                             <> value (AllowNewer [])
                                             <> metavar "PKG1:PKG2" )
  where
    readAllowNewer :: ReadM AllowNewer
    readAllowNewer = do
      allowNewerString <- str
      case parseAllowNewer allowNewerString of
        Just an -> return an
        Nothing -> readerError $
                     "Invalid allow-newer specification.\n\
                     \Should be of the form: pkg1:pkg2,*:base,..."

    parseAllowNewer :: String -> Maybe AllowNewer
    parseAllowNewer = fmap AllowNewer . traverse oneAllowNewer
                    . splitOn ','

    oneAllowNewer :: String -> Maybe (Text, Text)
    oneAllowNewer s
      | (Text.pack -> a, Text.pack . drop 1 -> b) <- break (== ':') s
      , a == "*" || validPackageName a
      , b == "*" || validPackageName b
      = Just (a,b)
      | otherwise
      = Nothing

-- | Utility list 'splitOn' function.
splitOn :: Char -> String -> [String]
splitOn c = go
  where
    go "" = []
    go s
      | (a,as) <- break (== c) s
      = a : go (drop 1 as)

-- | Parse a collection of package dependencies, either from a seed file
-- or from explicit command-line arguments.
dependencies :: ModeDescription -> Parser PackageData
dependencies modeDesc
  =   ( FromFile <$> seeds )
  <|> ( Explicit <$> pkgs )

  where

    seeds :: Parser FilePath
    seeds = option str ( long "seeds" <> help seedsHelp <> metavar "INFILE" )

    pkgs :: Parser PkgSpecs
    pkgs = M.fromList <$>
           some ( argument pkgSpec (metavar "PKG1 PKG2 ..." <> help pkgsHelp) )

    pkgSpec :: ReadM (PkgName, PkgSpec)
    pkgSpec = (,) <$> (PkgName <$> str)
                  <*> (PkgSpec Nothing <$> pure mempty)
                    -- TODO: flags & constraints not yet supported by
                    -- the command-line interface; use a SEED file instead.

    pkgsHelp, seedsHelp :: String
    (pkgsHelp, seedsHelp) = (what <> " seed packages", what <> " seed file")
      where
        what = modeDescription modeDesc

-- | Parse how we will obtain a build plan: by computing it, or by reading
-- from a @plan.json@ on disk?
plan :: ModeDescription -> Parser Plan
plan modeDesc = ( UsePlan <$> optPlanPath )
            <|> ( ComputePlan <$> planInputs modeDesc )

  where
    optPlanPath :: Parser FilePath
    optPlanPath =
      option str
        (  short 'p'
        <> long "plan"
        <> help "Input 'plan.json' file"
        <> metavar "INFILE" )

-- | Parse information about fetched sources: in which directory they belong,
-- and what build plan they correspond to.
fetchDescription :: ModeDescription -> Parser FetchDescription
fetchDescription modeDesc = do
  fetchInputPlan <- plan modeDesc
  fetchDir       <- optFetchDir
  return $ FetchDescription { fetchDir, fetchInputPlan }

  where
    optFetchDir :: Parser FilePath
    optFetchDir =
      option str
        (  short 'f'
        <> long "fetch-dir"
        <> help "Directory for fetched sources"
        <> metavar metavarStr )

    metavarStr :: String
    metavarStr = case modeDesc of
      Building -> "INDIR"
      _        -> "OUTDIR"

-- | Parse the options for the @build@ command.
build :: Parser Build
build = do

  buildFetchDescr <- fetchDescription Building
  buildFetch      <- optFetch
  buildStrategy   <- optStrategy
  buildOutputDir  <- optOutputDir

  return $ Build { buildFetch, buildFetchDescr
                 , buildStrategy, buildOutputDir }

  where

    optStrategy :: Parser BuildStrategy
    optStrategy =
      bool Async TopoSort <$>
        switch (  long "no-async"
               <> help "Disable asynchronous package building (useful for debugging)" )

    optFetch :: Parser Fetch
    optFetch =
      bool Fetch Prefetched <$>
        switch (  long "prefetched"
               <> help "Use prefetched sources instead of fetching from Hackage" )

    optOutputDir :: Parser FilePath
    optOutputDir =
      option str (  short 'o'
                 <> long "output-dir"
                 <> help "Output directory"
                 <> metavar "OUTDIR" )
