
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}

module Parse ( options, runOptionsParser ) where

-- base
import Data.Char
  ( isSpace )
import Data.Bool
  ( bool )

-- containers
import qualified Data.Map.Strict as Map
  ( empty, fromList, singleton )
import qualified Data.Set as Set
  ( empty, fromList, singleton )

-- optparse-applicative
import Options.Applicative

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( break, pack, unpack, words )

-- build-env
import CabalPlan
import Config
import Options
import Target

--------------------------------------------------------------------------------

-- | Run the command-line options parser.
runOptionsParser :: FilePath -> IO Opts
runOptionsParser currWorkDir =
  customExecParser ( prefs showHelpOnEmpty ) $
    info ( helper <*> options currWorkDir )
      (  fullDesc
      <> header "build-env - compute, fetch and build cabal build plans" )

-- | The command-line options parser for the 'build-env' executable.
options :: FilePath -> Parser Opts
options currWorkDir = do
  mode      <- optMode
  compiler  <- optCompiler
  cabal     <- optCabal
  workDir   <- optChangeWorkingDirectory currWorkDir
  verbosity <- optVerbosity
  delTemp   <- optTempDirPermanence
  return $ Opts { compiler, cabal, mode, verbosity, delTemp, workDir }

-- | Parse @ghc@ and @ghc-pkg@ paths.
optCompiler :: Parser Compiler
optCompiler =
    Compiler
      <$> option str (long "ghc" <> value "ghc" <> help "'ghc' executable path" <> metavar "GHC")
      <*> option str (long "ghc-pkg" <> value "ghc-pkg" <> help "'ghc-pkg' executable path" <> metavar "GHC-PKG")

-- | Parse @cabal@ path.
optCabal :: Parser Cabal
optCabal = do
  cabalPath <-
    option str
      (  long "cabal" <> value "cabal"
      <> help "'cabal' executable path"
      <> metavar "CABAL" )
  globalCabalArgs <-
    many $ option str (  long "cabal-arg"
                      <> help "Pass argument to 'cabal'"
                      <> metavar "ARG" )
  return $ Cabal { cabalPath, globalCabalArgs }

-- | Parse a @cwd@ (change working directory) option.
optChangeWorkingDirectory :: FilePath -> Parser FilePath
optChangeWorkingDirectory currWorkDir =
    option str
      (  long "cwd" <> value currWorkDir
      <> help "Set working directory"
      <> metavar "DIR" )

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
        info ( FetchMode <$> fetchDescription Fetching <*> newOrExisting )
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

-- | Text describing a mode of the @build-env@ executable.
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

  planUnits <- dependencies modeDesc
  planPins <- optional (freeze modeDesc)
  planAllowNewer <- allowNewer

  return $ PlanInputs { planPins, planUnits, planAllowNewer }

-- | Parse a list of pinned packages from a 'cabal.config' freeze file.
freeze :: ModeDescription -> Parser ( PackageData PkgSpecs )
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
                                             <> value (AllowNewer Set.empty)
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
    parseAllowNewer = fmap ( AllowNewer . Set.fromList )
                    . traverse oneAllowNewer
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

-- | Parse a collection of seed dependencies, either from a seed file
-- or from explicit command-line arguments.
dependencies :: ModeDescription -> Parser ( PackageData UnitSpecs )
dependencies modeDesc
  =   ( FromFile <$> seeds )
  <|> explicitUnits

  where

    seeds :: Parser FilePath
    seeds = option str ( long "seeds" <> help seedsHelp <> metavar "INFILE" )

    explicitUnits :: Parser ( PackageData UnitSpecs )
    explicitUnits = do
      units  <- some ( argument readUnitSpec (metavar "UNIT1 UNIT2 ..." <> help unitsHelp) )
      locals <- many localPkg
      return $ Explicit $
        foldl unionUnitSpecsCombining Map.empty units
         `unionUnitSpecsCombining`
           Map.fromList
             [ (pkg, (Local loc, emptyPkgSpec, mempty))
             | (pkg, loc) <- locals ]

    localPkg :: Parser (PkgName, FilePath)
    localPkg =
      option readLocalPkg
        ( long "local"
        <> help "Local package source location"
        <> metavar "\"PKG ABS_PATH\"" )

    readLocalPkg :: ReadM (PkgName, FilePath)
    readLocalPkg = do
      ln <- str
      return $ case Text.words ln of
        pkg:loc:_
          | validPackageName pkg
          -> (PkgName pkg, Text.unpack loc)
        _ -> error "Could not parse --local argument\n\
                   \Valid usage is of the form: --local \"PKG ABSOLUTE_PATH\""

    readUnitSpec :: ReadM UnitSpecs
    readUnitSpec = do
      ln <- str
      return $
        let (pkgTyComp, rest) = Text.break isSpace ln
            spec = parsePkgSpec rest
        in case parsePkgComponent pkgTyComp of
            Nothing -> error $ "Cannot parse package name: " <> Text.unpack ln
            Just (pkgNm, comp) ->
              Map.singleton pkgNm (Remote, spec, Set.singleton comp)

    unitsHelp, seedsHelp :: String
    (unitsHelp, seedsHelp) = (what <> " seed units", what <> " seed file")
      where
        what = modeDescription modeDesc

-- | Parse how we will obtain a build plan: by computing it, or by reading
-- from a @plan.json@ on disk?
plan :: ModeDescription -> Parser Plan
plan modeDesc = ( UsePlan <$> optPlanPath )
            <|> ( ComputePlan <$> planInputs modeDesc <*> optPlanOutput )

  where
    optPlanPath :: Parser FilePath
    optPlanPath =
      option str
        (  short 'p'
        <> long "plan"
        <> help "Input 'plan.json' file"
        <> metavar "INFILE" )

    optPlanOutput :: Parser (Maybe FilePath)
    optPlanOutput =
      option (fmap Just str)
        (  long "output-plan"
        <> value Nothing
        <> help "Output 'plan.json' file"
        <> metavar "OUTFILE" )

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

-- | Parse whether to create a new fetch directory or update an existing one.
newOrExisting :: Parser NewOrExisting
newOrExisting =
    bool New Existing <$>
      switch (  long "update"
             <> help "Update existing fetched sources directory" )

-- | Parse the options for the @build@ command.
build :: Parser Build
build = do

  buildFetchDescr <- fetchDescription Building
  buildFetch      <- optFetch
  buildStrategy   <- optStrategy
  buildDestDir    <- optDestDir
  configureArgs   <- optConfigureArgs
  ghcPkgArgs      <- optGhcPkgArgs

  return $ Build { buildFetch, buildFetchDescr
                 , buildStrategy, buildDestDir
                 , configureArgs, ghcPkgArgs }

  where

    optStrategy :: Parser BuildStrategy
    optStrategy = async <|> script <|> pure TopoSort

    async :: Parser BuildStrategy
    async = flag' Async
        (  long "async"
        <> help "Use asynchronous package building" )

    script :: Parser BuildStrategy
    script = option (Script <$> str)
        (  long "script"
        <> help "Output a shell script containing build steps"
        <> metavar "OUTFILE" )

    optFetch :: Parser Fetch
    optFetch =
      prefetched <|> ( Fetch <$> newOrExisting )

    prefetched :: Parser Fetch
    prefetched =
      flag' Prefetched
        (  long "prefetched"
        <> help "Use prefetched sources instead of fetching from Hackage" )

    optDestDir :: Parser (DestDir Raw)
    optDestDir = do
      prefix <-
        option str (  short 'o'
                   <> long "prefix"
                   <> help "Installation prefix"
                   <> metavar "OUTDIR" )
      destDir <-
        option str (  long "dest-dir"
                   <> help "Installation destination directory"
                   <> value "/"
                   <> metavar "OUTDIR" )
      return $
        DestDir
          { destDir, prefix
          , installDir = () -- computed after path canonicalisation
          }

    -- TODO: this only supports passing @setup configure@ arguments
    -- for all packages at once, rather than per-package.
    optConfigureArgs :: Parser TargetArgs
    optConfigureArgs = do
      args <- many $ option str (  long "configure-arg"
                                <> help "Pass argument to 'Setup configure'"
                                <> metavar "ARG" )
      return $ TargetArgs $ Map.singleton AllPkgs args

    optGhcPkgArgs :: Parser Args
    optGhcPkgArgs =
      many $ option str (  long "ghc-pkg-arg"
                        <> help "Pass argument to 'ghc-pkg register'"
                        <> metavar "ARG" )
