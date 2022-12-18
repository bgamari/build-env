
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Word
  ( Word8 )

-- containers
import qualified Data.Map.Strict as Map
  ( empty, fromList, singleton )
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

--------------------------------------------------------------------------------

-- | Run the command-line options parser.
runOptionsParser :: FilePath -> IO Opts
runOptionsParser currWorkDir =
  customExecParser ( prefs config ) $
    info ( helper <*> options currWorkDir )
      (  fullDesc
      <> header "build-env - compute, fetch and build Cabal build plans" )

  where
    config =
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
  return $ Opts { compiler, cabal, mode, verbosity, delTemp, workDir }

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
  return $ Cabal { cabalPath, globalCabalArgs }

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
        ( progDesc "Compute a build plan from a collection of seeds" )
    , command "fetch" $
        info ( FetchMode <$> fetchDescription <*> newOrExisting )
        ( fullDesc <> progDesc "Fetch package sources" )
    , command "build" $
        info ( BuildMode <$> build )
        ( fullDesc <> progDesc "Build and register packages" )
    ]
  where
    optOutput :: Parser FilePath
    optOutput =
      option str (  short 'p'
                 <> long "plan"
                 <> help "Output 'plan.json' file"
                 <> metavar "OUTFILE"
                 )


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
      units  <- some ( argument readUnitSpec (metavar "UNIT" <> help unitsHelp) )
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
  return $ FetchDescription { rawFetchDir, fetchInputPlan }

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
  buildStrategy  <- optStrategy
  buildRawPaths  <- optRawPaths
  buildFetch     <- optFetch
  userUnitArgs   <- optUnitArgs

  return $
    Build { buildFetch
          , buildBuildPlan
          , buildStrategy
          , buildRawPaths
          , userUnitArgs }

  where

    optStrategy :: Parser BuildStrategy
    optStrategy =
      script <|> async <|> pure (Execute TopoSort)

    async :: Parser BuildStrategy
    async = Execute . Async <$>
      option (auto @Word8 <|> return 0)
        (  short 'j'
        <> long "async"
        <> help "Use asynchronous package building"
        <> metavar "NUM_THREADS" )

    script :: Parser BuildStrategy
    script = do
      scriptPath <-
        option str
          (  long "script"
          <> help "Output a shell script containing build steps"
          <> metavar "OUTFILE" )
      useVariables <-
        switch
         (  long "variables"
         <> help "Use variables in the shell script output ($PREFIX etc)" )
      return $ Script { scriptPath, useVariables }

    optFetch :: Parser Fetch
    optFetch =
      prefetched <|> ( Fetch <$> newOrExisting )

    prefetched :: Parser Fetch
    prefetched =
      flag' Prefetched
        (  long "prefetched"
        <> help "Use prefetched sources instead of fetching from Hackage" )

    optRawPaths :: Parser ( Paths Raw )
    optRawPaths = do
      fetchDir <- optFetchDir Building
      rawPrefix <-
        option str (  short 'o'
                   <> long "prefix"
                   <> help "Installation prefix"
                   <> value "install"
                       -- Having this default value is helpful when outputting
                       -- a shell script that uses variables, as in that case
                       -- we ignore it completely, so it's annoying to require
                       -- the user to specify it.
                       --
                       -- It would be better to make this optional conditional
                       -- on whether the user has passed --variables, but
                       -- that can't be easily done with optparse-applicative.
                   <> metavar "OUTDIR" )
      rawDestDir <-
        option str (  long "destdir"
                   <> help "Installation destination directory"
                   <> value "/"
                   <> metavar "OUTDIR" )
      pure $ Paths { fetchDir, buildPaths = RawBuildPaths { rawPrefix, rawDestDir } }

    -- TODO: we only support passing arguments for all units at once,
    -- rather than per-unit.
    optUnitArgs :: Parser ( ConfiguredUnit -> UnitArgs )
    optUnitArgs = do
      confArgs  <- optConfigureArgs
      mbDocArgs <- optHaddockArgs
      regArgs   <- optGhcPkgArgs
      return $ \ cu ->
        UnitArgs { configureArgs = confArgs cu
                 , mbHaddockArgs = fmap ($ cu) mbDocArgs
                 , registerArgs  = regArgs cu }

    optConfigureArgs :: Parser ( ConfiguredUnit -> Args )
    optConfigureArgs = do
      args <- many $ option str (  long "configure-arg"
                                <> help "Pass argument to 'Setup configure'"
                                <> metavar "ARG" )
      return $ const args

    optHaddockArgs :: Parser ( Maybe ( ConfiguredUnit -> Args ) )
    optHaddockArgs = do
      doHaddock <-
        bool False True <$>
          switch (  long "haddock"
                 <> help "Generate haddock documentation" )
      args <- many $
        option str (  long "haddock-arg"
                   <> help "Pass argument to 'Setup haddock'"
                   <> metavar "ARG" )
      return $
        if null args && not doHaddock
        then Nothing
        else Just $ const args

    optGhcPkgArgs :: Parser ( ConfiguredUnit -> Args )
    optGhcPkgArgs = do
      args <- many $
        option str (  long "ghc-pkg-arg"
                   <> help "Pass argument to 'ghc-pkg register'"
                   <> metavar "ARG" )
      return $ const args
