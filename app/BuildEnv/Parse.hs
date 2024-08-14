
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  BuildEnv.Parse
-- Description :  Command-line option parser for @build-env@
--
-- This module implements the command-line options parsing for @build-env@.
module BuildEnv.Parse ( options, runOptionsParser ) where

-- base
import Data.Char
  ( isSpace )
import Data.Bool
  ( bool )
import System.Environment
  ( getArgs )
import Text.Read
  ( readMaybe )

-- containers
import qualified Data.Map.Strict as Map
  ( empty, fromList, singleton )
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( fromList, singleton )

-- optparse-applicative
import Options.Applicative

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( break, pack, splitOn, unpack )

-- build-env
import BuildEnv.CabalPlan
import BuildEnv.Config
import BuildEnv.Options
import BuildEnv.Utils
  ( splitOn )

--------------------------------------------------------------------------------

-- | Run the command-line options parser.
runOptionsParser :: FilePath -> IO Opts
runOptionsParser currWorkDir = do
  args <- getArgs
  handleParseResult $ execParserPure pPrefs pInfo args
  where
    pInfo =
      info ( helper <*> options currWorkDir )
        (  fullDesc
        <> header "build-env - compute, fetch and build Cabal build plans" )
    pPrefs = prefs $
      mconcat [ showHelpOnEmpty
              , subparserInline
              , helpShowGlobals
              , multiSuffix "*"
              , columns 90 ]

-- | The command-line options parser for the 'build-env' executable.
options :: FilePath -> Parser Opts
options currWorkDir = do
  mode      <- optMode
  compiler  <- optCompiler
  cabal     <- optCabal
  workDir   <- optChangeWorkingDirectory currWorkDir
  verbosity <- optVerbosity
  delTemp   <- optTempDirPermanence
  pure $ Opts { compiler, cabal, mode, verbosity, delTemp, workDir }

-- | Parse @ghc@ and @ghc-pkg@ paths.
optCompiler :: Parser Compiler
optCompiler =
    Compiler
      <$> option str
            (  long "ghc"
            <> value "ghc"
            <> help "'ghc' executable path"
            <> metavar "GHC"
            )
      <*> option str
            (  long "ghc-pkg"
            <> value "ghc-pkg"
            <> help "'ghc-pkg' executable path"
            <> metavar "GHC-PKG"
            )

-- | Parse @cabal@ path.
optCabal :: Parser Cabal
optCabal = do
  cabalPath <-
    option str
      (  long "cabal"
      <> value "cabal"
      <> help "'cabal' executable path"
      <> metavar "CABAL" )
  globalCabalArgs <-
    many $ option str (  long "cabal-arg"
                      <> help "Pass argument to 'cabal'"
                      <> metavar "ARG" )
  pure $ Cabal { cabalPath, globalCabalArgs }

-- | Parse a @cwd@ (change working directory) option.
optChangeWorkingDirectory :: FilePath -> Parser FilePath
optChangeWorkingDirectory currWorkDir =
    option str
      (  long "cwd"
      <> value currWorkDir
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
        ( fullDesc <> planInfo )
    , command "fetch" $
        info ( FetchMode <$> fetchDescription <*> newOrExisting )
        ( fullDesc <> fetchInfo )
    , command "build" $
        info ( BuildMode <$> build )
        ( fullDesc <> buildInfo )
    ]
  where
    optOutput :: Parser FilePath
    optOutput =
      option str (  short 'p'
                 <> long "plan"
                 <> help "Output 'plan.json' file"
                 <> metavar "OUTFILE"
                 )
    planInfo, fetchInfo, buildInfo :: InfoMod a
    planInfo  = progDesc "Compute a build plan from a collection of seeds"
    fetchInfo = progDesc "Fetch package sources"
    buildInfo = progDesc "Build and register packages"

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

  planPins <- optional (freeze modeDesc)
  planUnits <- dependencies modeDesc
  planAllowNewer <- allowNewer
  planIndexState <- indexState

  pure $ PlanInputs { planPins, planUnits, planAllowNewer, planIndexState }

-- | Parse a list of pinned packages from a 'cabal.config' freeze file.
freeze :: ModeDescription -> Parser ( PackageData PkgSpecs )
freeze modeDesc = FromFile <$> freezeFile
  where
    freezeFile :: Parser FilePath
    freezeFile = option str ( long "freeze" <> help helpStr <> metavar "INFILE" )

    helpStr :: String
    helpStr = modeDescription modeDesc <> " 'cabal.config' freeze file"

indexState :: Parser ( Maybe IndexState )
indexState =
  optional $ fmap ( IndexState . Text.pack ) $
    option str ( long "index-state" <> help helpStr <> metavar "DATE" )
  where
    helpStr = "use Hackage state as of DATE, e.g. 2022-12-25T00:00:00Z"

-- | Parse @allow-newer@ options.
allowNewer :: Parser AllowNewer
allowNewer =
  fmap mconcat . many $
    option readAllowNewer
      (  long "allow-newer" <> help "Allow-newer specification"
      <> metavar "PKG1:PKG2" )
  where
    readAllowNewer :: ReadM AllowNewer
    readAllowNewer = do
      allowNewerString <- str
      case parseAllowNewer allowNewerString of
        Just an -> return an
        Nothing ->
          readerError $
            "Invalid allow-newer specification.\n" ++
            "Should be of the form: pkg1:pkg2,*:base,..."

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
      units  <- some ( argument readUnitSpec (metavar "UNIT" <> help unitsHelp) )
      locals <- many localPkg
      pure $ Explicit $
        foldl unionUnitSpecsCombining Map.empty units
         `unionUnitSpecsCombining`
           Map.fromList
             [ (pkg, (Local loc, emptyPkgSpec, mempty))
             | (pkg, loc) <- locals ]

    localPkg :: Parser (PkgName, FilePath)
    localPkg =
      option readLocalPkg
        (  long "local"
        <> help "Local package source location"
        <> metavar "PKG=PATH" )

    readLocalPkg :: ReadM (PkgName, FilePath)
    readLocalPkg = do
      ln <- str
      case Text.splitOn "=" ln of
        pkg:loc:_
          | validPackageName pkg
          -> return (PkgName pkg, Text.unpack loc)
        _ -> readerError $
              "Could not parse --local argument\n" ++
              "Valid usage is of the form: --local PKG=PATH"

    readUnitSpec :: ReadM UnitSpecs
    readUnitSpec = do
      ln <- str
      let (pkgTyComp, rest) = Text.break isSpace ln
          spec = parsePkgSpec rest
      case parsePkgComponent pkgTyComp of
          Nothing -> readerError $ "Cannot parse package name: " <> Text.unpack ln
          Just (pkgNm, comp) ->
            return $ Map.singleton pkgNm (Remote, spec, Set.singleton comp)

    unitsHelp, seedsHelp :: String
    (unitsHelp, seedsHelp) = (what <> " seed unit", what <> " seed file")
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
fetchDescription :: Parser FetchDescription
fetchDescription = do
  fetchInputPlan <- plan Fetching
  rawFetchDir    <- optFetchDir Fetching
  pure $ FetchDescription { rawFetchDir, fetchInputPlan }

-- | Parse the fetch directory.
optFetchDir :: ModeDescription -> Parser FilePath
optFetchDir modeDesc =
  option str
    (  short 'f'
    <> long "fetchdir"
    <> help "Directory for fetched sources"
    <> metavar metavarStr )
  where
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

  buildBuildPlan <- plan Building
  buildStart     <- optStart
  buildStrategy  <- optStrategy
  buildRawPaths  <- optRawPaths
  userUnitArgs   <- optUnitArgs
  mbOnlyDepsOf   <- optOnlyDepsOf
  eventLogDir    <- optEventLogDir
  pure $
    Build { buildStart
          , buildBuildPlan
          , buildStrategy
          , buildRawPaths
          , userUnitArgs
          , mbOnlyDepsOf
          , eventLogDir }

  where

    optStrategy :: Parser BuildStrategy
    optStrategy = optScript <|> ( Execute <$> optRunStrat )

    optRunStrat :: Parser RunStrategy
    optRunStrat = ( Async <$> asyncSem ) <|> pure TopoSort

    asyncSem :: Parser AsyncSem
    asyncSem =
      option jsem
        (  long "jsem"
        <> help "Use a system semaphore to control parallelism"
        <> metavar "[N|SEM_NAME]" )
      <|>
      option ( fmap NewQSem auto )
        (  short 'j'
        <> help "Use asynchronous package building"
        <> metavar "N" )
      <|>
      pure NoSem

    jsem :: ReadM AsyncSem
    jsem = do
      x <- str
      return $
        case readMaybe x of
          Just n  -> NewJSem n
          Nothing -> ExistingJSem x

    optScript :: Parser BuildStrategy
    optScript = do
      scriptPath <- optScriptPath
      useVariables <-
        switch
          (  long "variables"
          <> help "Use variables in the shell script output ($PREFIX etc)" )
      pure $ Script { scriptPath, useVariables }

    optScriptPath :: Parser FilePath
    optScriptPath =
      option str
        (  long "script"
        <> help "Output a shell script containing build steps"
        <> metavar "OUTFILE" )

    optStart :: Parser BuildStart
    optStart =
      resume <|> prefetched <|> ( Fetch <$> newOrExisting )

    resume :: Parser BuildStart
    resume =
      flag' Resume
        (  long "resume"
        <> help "Resume a partially-completed build" )

    prefetched :: Parser BuildStart
    prefetched =
      flag' Prefetched
        (  long "prefetched"
        <> help "Start the build from the prefetched sources" )

    optRawPaths :: Parser (Paths Raw)
    optRawPaths = do
      fetchDir   <- optFetchDir Building
      buildPaths <- optBuildPaths
      return $ Paths { fetchDir, buildPaths }

    optBuildPaths :: Parser (BuildPaths Raw)
    optBuildPaths = do
      rawPrefix <-
        option str (  short 'o'
                   <> long "prefix"
                   <> help "Installation prefix"
                   <> metavar "OUTDIR" )
      rawDestDir <-
        option str (  long "destdir"
                   <> help "Installation destination directory"
                   <> value "/"
                   <> metavar "OUTDIR" )
      pure $ RawBuildPaths { rawPrefix, rawDestDir }

    -- TODO: we only support passing arguments for all units at once,
    -- rather than per-unit.
    optUnitArgs :: Parser ( ConfiguredUnit -> UnitArgs )
    optUnitArgs = do
      confArgs  <- optConfigureArgs
      mbDocArgs <- optHaddockArgs
      regArgs   <- optGhcPkgArgs
      pure $ \ cu ->
        UnitArgs { configureArgs = confArgs cu
                 , mbHaddockArgs = fmap ($ cu) mbDocArgs
                 , registerArgs  = regArgs cu }

    optConfigureArgs :: Parser ( ConfiguredUnit -> Args )
    optConfigureArgs = do
      args <- many $ option str (  long "configure-arg"
                                <> help "Pass argument to 'Setup configure'"
                                <> metavar "ARG" )
      pure $ const args

    optHaddockArgs :: Parser ( Maybe ( ConfiguredUnit -> Args ) )
    optHaddockArgs = do
      doHaddock <-
        switch (  long "haddock"
               <> help "Generate haddock documentation" )
      args <- many $
        option str (  long "haddock-arg"
                   <> help "Pass argument to 'Setup haddock'"
                   <> metavar "ARG" )
      pure $
        if null args && not doHaddock
        then Nothing
        else Just $ const args

    optGhcPkgArgs :: Parser ( ConfiguredUnit -> Args )
    optGhcPkgArgs = do
      args <- many $
        option str (  long "ghc-pkg-arg"
                   <> help "Pass argument to 'ghc-pkg register'"
                   <> metavar "ARG" )
      pure $ const args

    optOnlyDepsOf :: Parser ( Maybe ( Set PkgName ) )
    optOnlyDepsOf = do
      pkgs <- many $
        option pkgName
          (  long "only"
          <> help "Only build these packages (and their dependencies)"
          <> metavar "PKG" )
      pure $
        if null pkgs
        then Nothing
        else Just $ Set.fromList pkgs

    pkgName :: ReadM PkgName
    pkgName = do
      nm <- str
      if validPackageName nm
      then return $ PkgName nm
      else readerError $
            "Invalid package name: " <> Text.unpack nm

    optEventLogDir :: Parser ( Maybe FilePath )
    optEventLogDir = do
        option (fmap Just str)
          (  long "eventlogs"
          <> help "Directory for GHC eventlogs"
          <> value Nothing
          <> metavar "OUTDIR" )
