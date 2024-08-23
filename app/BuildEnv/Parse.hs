
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
import Control.Concurrent
  ( getNumCapabilities )
import Data.Char
  ( isSpace )
import Data.Bool
  ( bool )
import Data.Word
  ( Word16 )
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
import Options.Applicative.Help.Pretty
  ( (.$.) )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( break, pack, splitOn, unpack )

-- build-env
import BuildEnv.CabalPlan
import BuildEnv.Config
import BuildEnv.Options
import BuildEnv.Path
import BuildEnv.Utils
  ( splitOn )

--------------------------------------------------------------------------------

-- | Run the command-line options parser.
runOptionsParser :: IO ( GlobalOpts, Mode )
runOptionsParser = do
  args <- getArgs
  caps <- NumCapabilities . fromIntegral <$> getNumCapabilities
  let
    pInfo =
      info ( helper <*> options caps )
        (  fullDesc
        <> header "build-env - compute, fetch and build Cabal build plans" )
  handleParseResult $ execParserPure pPrefs pInfo args
  where
    pPrefs = prefs $
      mconcat [ showHelpOnEmpty
              , subparserInline
              , helpShowGlobals
              , multiSuffix "*"
              , columns 90 ]

newtype NumCapabilities = NumCapabilities { numCapabilities :: Word16 }
  deriving stock ( Show, Eq, Ord )

-- | The command-line options parser for the 'build-env' executable.
options :: NumCapabilities -> Parser ( GlobalOpts, Mode )
options caps = do
  mode       <- optMode caps
  globalOpts <- optGlobalOpts
  pure ( globalOpts, mode )

-- | Parse global options for 'build-env'.
optGlobalOpts :: Parser GlobalOpts
optGlobalOpts = do
  compiler   <- optCompiler
  cabal      <- optCabal
  workDir    <- optChangeWorkingDirectory
  verbosity  <- optVerbosity
  indexState <- optIndexState
  delTemp    <- optTempDirPermanence
  pure $
    GlobalOpts
      { compiler, cabal
      , verbosity, delTemp
      , workDir, indexState
      }

-- | Parse @ghc@ and @ghc-pkg@ paths.
optCompiler :: Parser Compiler
optCompiler =
    Compiler
      <$> option ( fmap mkAbsolutePath str )
            (  long "ghc"
            <> value ( mkAbsolutePath "ghc" )
            <> help "'ghc' executable path"
            <> metavar "GHC"
            )
      <*> option ( fmap mkAbsolutePath str )
            (  long "ghc-pkg"
            <> value ( mkAbsolutePath "ghc-pkg" )
            <> help "'ghc-pkg' executable path"
            <> metavar "GHC-PKG"
            )

-- | Parse @cabal@ path.
optCabal :: Parser Cabal
optCabal = do
  cabalPath <-
    option ( fmap mkAbsolutePath str )
      (  long "cabal"
      <> value ( mkAbsolutePath "cabal" )
      <> help "'cabal' executable path"
      <> metavar "CABAL" )
  globalCabalArgs <-
    many $ option str (  long "cabal-arg"
                      <> help "Pass argument to 'cabal'"
                      <> metavar "ARG" )
  pure $ Cabal { cabalPath, globalCabalArgs }

-- | Parse a @cwd@ (change working directory) option.
optChangeWorkingDirectory :: Parser ( SymbolicPath CWD ( Dir Project ) )
optChangeWorkingDirectory =
    option ( fmap mkSymbolicPath str )
      (  long "cwd"
      <> value sameDirectory
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

-- | Parse a Hackage index state.
optIndexState :: Parser ( Maybe IndexState )
optIndexState =
  optional $ fmap ( IndexState . Text.pack ) $
    option str ( long "index-state" <> help helpStr <> metavar "DATE" )
  where
    helpStr = "Use Hackage state as of DATE, e.g. 2022-12-25T00:00:00Z"

-- | Parse the mode in which to run the application: plan, fetch, build.
optMode :: NumCapabilities -> Parser Mode
optMode caps =
  hsubparser . mconcat $
    [ command "plan"  $
        info ( PlanMode  <$> planInputs Planning <*> optOutput )
        ( fullDesc <> planInfo )
    , command "fetch" $
        info ( FetchMode <$> fetchDescription <*> newOrExisting )
        ( fullDesc <> fetchInfo )
    , command "build" $
        info ( BuildMode <$> build caps )
        ( fullDesc <> buildInfo )
    ]
  where
    optOutput :: Parser ( SymbolicPath Project File )
    optOutput =
      option ( fmap mkSymbolicPath str )
        (  short 'p'
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

  pure $ PlanInputs { planPins, planUnits, planAllowNewer }

-- | Parse a list of pinned packages from a 'cabal.config' freeze file.
freeze :: ModeDescription -> Parser ( PackageData PkgSpecs )
freeze modeDesc = FromFile <$> freezeFile
  where
    freezeFile :: Parser ( SymbolicPath Project File )
    freezeFile = option ( fmap mkSymbolicPath str )
                   ( long "freeze" <> help helpStr <> metavar "INFILE" )

    helpStr :: String
    helpStr = modeDescription modeDesc <> " 'cabal.config' freeze file"

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

    seeds :: Parser ( SymbolicPath Project File )
    seeds = option ( fmap mkSymbolicPath str )
              ( long "seeds" <> help seedsHelp <> metavar "INFILE" )

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

    localPkg :: Parser ( PkgName, SymbolicPath Project ( Dir Pkg ) )
    localPkg =
      option readLocalPkg
        (  long "local"
        <> help "Local package source location"
        <> metavar "PKG=PATH" )

    readLocalPkg :: ReadM ( PkgName, SymbolicPath Project ( Dir Pkg ) )
    readLocalPkg = do
      ln <- str
      case Text.splitOn "=" ln of
        pkg:loc:_
          | validPackageName pkg
          -> return ( PkgName pkg, mkSymbolicPath $ Text.unpack loc )
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
    optPlanPath :: Parser ( SymbolicPath Project File )
    optPlanPath =
      option ( fmap mkSymbolicPath str )
        (  short 'p'
        <> long "plan"
        <> help "Input 'plan.json' file"
        <> metavar "INFILE" )

    optPlanOutput :: Parser ( Maybe ( SymbolicPath Project File ) )
    optPlanOutput =
      option ( fmap ( Just . mkSymbolicPath ) str )
        (  long "output-plan"
        <> value Nothing
        <> help "Output 'plan.json' file"
        <> metavar "OUTFILE" )

-- | Parse information about fetched sources: in which directory they belong,
-- and what build plan they correspond to.
fetchDescription :: Parser FetchDescription
fetchDescription = do
  fetchInputPlan <- plan Fetching
  fetchDir       <- optFetchDir Fetching
  pure $ FetchDescription { fetchDir, fetchInputPlan }

-- | Parse the fetch directory.
optFetchDir :: ModeDescription -> Parser ( SymbolicPath Project ( Dir Fetch ) )
optFetchDir modeDesc =
  option ( fmap mkSymbolicPath str )
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
build :: NumCapabilities -> Parser Build
build ( NumCapabilities numCaps ) = do

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
      option ( fmap NewQSem j )
        (  short 'j'
        <> help "Use asynchronous package building"
        <> metavar "[N]" )

    j :: ReadM Word16
    j = do
      x <- str
      if null x
      then return numCaps
      else
        case readMaybe x of
          Just n -> return n
          Nothing ->
           readerError $
             "Invalid -j argument: " <> x

    jsem :: ReadM AsyncSem
    jsem = do
      x <- str
      return $
        if null x
        then NewJSem numCaps
        else
          case readMaybe x of
            Just n  -> NewJSem n
            Nothing -> ExistingJSem x

    optScript :: Parser BuildStrategy
    optScript = do
      scriptPath   <- optScriptPath
      scriptType   <- optScriptType
      useVariables <-
        switch
          (  long "variables"
          <> help "Use variables in the shell script output ($PREFIX etc)" )
      pure $ GenerateScript { scriptPath, scriptType, useVariables }

    optScriptPath :: Parser ( SymbolicPath CWD File )
    optScriptPath =
      option ( fmap mkSymbolicPath str )
        (  long "script"
        <> helpDoc ( Just $  "Output a shell script containing build steps"
                         .$. "  NB: path is interpreted relative to current work dir, NOT --cwd" )
        <> metavar "OUTFILE" )

    optScriptType :: Parser ScriptType
    optScriptType =
      flag' Ninja
        (  long "ninja"
        <> help "Generate a ninja build script" )
      <|> pure Shell

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

    optRawPaths :: Parser ( Paths Raw )
    optRawPaths = do
      fetchDir   <- optFetchDir Building
      buildPaths <- optBuildPaths
      return $ Paths { fetchDir, buildPaths }

    optBuildPaths :: Parser (BuildPaths Raw)
    optBuildPaths = do
      rawPrefix <-
        option ( fmap mkSymbolicPath str )
          (  short 'o'
          <> long "prefix"
          <> help "Installation prefix"
          <> metavar "OUTDIR" )
      rawDestDir <-
        option ( fmap mkSymbolicPath str )
          (  long "destdir"
          <> help "Installation destination directory"
          <> value ( mkSymbolicPath "/" )
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
               <> help "Generate Haddock documentation" )
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

    optEventLogDir :: Parser ( Maybe ( SymbolicPath Project ( Dir Logs ) ) )
    optEventLogDir = do
        option ( fmap ( Just . mkSymbolicPath ) str )
          (  long "eventlogs"
          <> help "Directory for GHC event logs"
          <> value Nothing
          <> metavar "OUTDIR" )
