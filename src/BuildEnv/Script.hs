{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  BuildEnv.Script
-- Description :  Tiny build script DSL
--
-- This modules provides a tiny build script DSL.
--
-- A 'BuildScript' is a series of simple build steps (process calls).
--
-- A 'BuildScript' can be executed in the 'IO' monad, using 'executeBuildScript'.
--
-- A 'BuildScript' can be turned into a shell script which can be executed
-- later, using 'script'.
module BuildEnv.Script
  ( -- * Interpreting build scripts

    -- ** Executing build scripts
    executeBuildScript
  , executeBuildScriptNoProcessEnv

    -- ** Shell-script output
  , shellScriptHeader
  , shellScriptSteps

    -- * Build scripts
  , BuildScript, BuildScriptM(..)
  , askScriptConfig
  , buildSteps

    -- ** Individual build steps
  , BuildStep(..), BuildSteps
  , step
  , callProcess, createDir
  , logMessage, reportProgress
  , stepScript
  , setProcessEnvSteps
  , callProcessStep

    -- ** Configuring build scripts
  , ScriptOutput(..), ScriptConfig(..), hostRunCfg
  , EscapeVars(..), quoteArg

  , q

  ) where

-- base
import Control.Monad
  ( when )
import Data.Foldable
  ( traverse_, foldl', for_ )
import Data.IORef
  ( atomicModifyIORef' )
import Data.List
  ( partition )
import Data.Monoid
  ( Ap(..) )
import Data.String
  ( IsString(..) )
import System.IO
  ( hFlush )
import qualified System.IO as System
  ( stdout )

-- directory
import System.Directory
  ( createDirectoryIfMissing )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text

-- transformers
import Control.Monad.Trans.Reader
  ( ReaderT(..) )
import Control.Monad.Trans.Writer.CPS
  ( Writer, runWriter, tell )

-- build-env
import BuildEnv.Config
  ( Compiler(..), Cabal(..)
  , Verbosity(..), Counter(..)
  , ScriptType(..)
  , ProcessEnv(..)
  , Style(..), hostStyle
  )
import BuildEnv.Path
import BuildEnv.Utils
  ( CallProcess(..), callProcessInIO
  , programPath
  )

--------------------------------------------------------------------------------
-- Build scripts: general monad setup.

-- | A build script: a list of build steps, given a 'ScriptConfig' context.
type BuildScript dir a = BuildScriptM dir a

deriving via Ap ( BuildScriptM dir ) ()
  instance Semigroup ( BuildScript dir () )
deriving via Ap ( BuildScriptM dir ) ()
  instance Monoid ( BuildScript dir () )

-- | Build script monad.
newtype BuildScriptM dir a =
  BuildScript
    { runBuildScript :: ReaderT ScriptConfig ( Writer ( BuildSteps dir ) ) a }
  deriving newtype ( Functor, Applicative, Monad )

-- | Retrieve the 'ScriptConfig' from the 'ReaderT' environment.
askScriptConfig :: BuildScriptM dir ScriptConfig
askScriptConfig = BuildScript $ ReaderT return

-- | Obtain the build steps of a 'BuildScript'.
buildSteps :: ScriptConfig -> BuildScript dir a -> ( a, BuildSteps dir )
buildSteps cfg buildScript
  = runWriter ( runBuildScript buildScript `runReaderT` cfg )

--------------------------------------------------------------------------------
-- Individual build steps

-- | A list of build steps.
type BuildSteps dir = [ BuildStep dir ]

-- | A build step.
data BuildStep dir
  -- | Call a processs with the given arguments.
  = CallProcess ( CallProcess dir )
  -- | Create the given directory.
  | forall to. CreateDir ( AbsolutePath ( Dir to ) )
  -- | Log a message to @stdout@.
  | LogMessage  String
  -- | Report one unit of progress.
  | ReportProgress
      { outputProgress :: Bool
        -- ^ Whether to log the progress to @stdout@.
      }

-- | Declare a build step.
step :: BuildStep dir -> BuildScript dir ()
step s = BuildScript $ ReaderT \ _ -> tell [s]

-- | Call a process with given arguments.
callProcess :: CallProcess dir -> BuildScript dir ()
callProcess = step . CallProcess

-- | Create the given directory.
createDir :: AbsolutePath ( Dir to ) -> BuildScript dir ()
createDir = step . CreateDir

-- | Log a message.
logMessage :: Verbosity -> Verbosity -> String -> BuildScript dir ()
logMessage v msg_v msg
  | v >= msg_v
  = step $ LogMessage msg
  | otherwise
  = return ()

-- | Report one unit of progress.
reportProgress :: Verbosity -> BuildScript dir ()
reportProgress v = step ( ReportProgress { outputProgress = v > Quiet } )

--------------------------------------------------------------------------------
-- Configuration

-- | How to interpret the build script: run it in 'IO', or turn it
-- into a shell script?
data ScriptOutput
  -- | Run the build script in 'IO'
  = Run
  -- | Generate a shell script.
  | Script
    { useVariables :: !Bool
      -- ^ Replace various values with variables, so that
      -- they can be set before running the build script.
      --
      -- Values:
      --
      --  - @GHC@ and @GHC-PKG@,
      --  - fetched sources directory @SOURCES@,
      --  - @PREFIX@ and @DESTDIR@.
    , scriptType   :: !ScriptType
    }

-- | Configuration options for a 'BuildScript'.
data ScriptConfig
  = ScriptConfig
  { scriptOutput :: !ScriptOutput
    -- ^ Whether we are outputting a shell script, so that we can know whether
    -- we should:
    --
    --  - add quotes around command-line arguments?
    --  - add @./@ to run an executable in the current working directory?

  , scriptStyle :: !Style
    -- ^ Whether to use Posix or Windows style conventions. See 'Style'.

  , scriptTotal :: !( Maybe Word )
    -- ^ Optional: the total number of units we are building;
    -- used to report progress.
  }

-- | Configure a script to run on the host (in @IO@).
hostRunCfg :: Maybe Word -- ^ Optional: total to report progress against.
           -> ScriptConfig
hostRunCfg mbTotal =
  ScriptConfig
    { scriptOutput     = Run
    , scriptStyle      = hostStyle
    , scriptTotal      = mbTotal
    }

-- | Whether to expand or escape variables in a shell script.
data EscapeVars
  -- | Allow the shell to expand variables.
  = ExpandVars
  -- | Escape variables so that the shell doesn't expand them.
  | EscapeVars
  deriving stock Show

-- | Quote a string, to avoid spaces causing the string
-- to be interpreted as multiple arguments.
q :: ( IsString r, Monoid r ) => ScriptType -> EscapeVars -> String -> r
q scriptType escapeVars t = "\"" <> fromString ( escapeArg t ) <> "\""
  where
    escapeArg :: String -> String
    escapeArg = reverse . foldl' escape []

    charsToEscape :: [ Char ]
    charsToEscape = case escapeVars of
      EscapeVars
        | scriptType /= Ninja
        -- Don't escape variables for Ninja scripts.
        -> [ '\\', '\'', '"', '$' ]
      _ -> [ '\\', '\'', '"' ]

    escape :: String -> Char -> String
    escape cs c
      | c `elem` charsToEscape
      = c:'\\':cs
      | otherwise
      = c:cs

-- | Quote a command-line argument, if the 'ScriptConfig' requires arguments
-- to be quoted.
--
-- No need to call this on the 'cwd' or 'prog' fields of 'CallProcess',
-- as these will be quoted by the shell-script backend no matter what.
quoteArg :: ( IsString r, Monoid r )
         => EscapeVars
         -> ScriptConfig
         -> String
         -> r
quoteArg escapeVars ( ScriptConfig { scriptOutput } ) =
  case scriptOutput of
    Run -> fromString
    Script { scriptType } ->
      q scriptType escapeVars

--------------------------------------------------------------------------------
-- Interpretation

------
-- IO

-- | Execute a 'BuildScript' in the 'IO' monad.
executeBuildScript :: forall dir
                   .  Compiler
                   -> Cabal
                   -> Maybe Counter
                        -- ^ Optional counter to use to report progress.
                   -> BuildScript dir ( ProcessEnv dir )
                        -- ^ The build script to execute.
                   -> IO ()
executeBuildScript compiler cabal counter script = do
  let ( procEnv, steps ) = buildSteps ( hostRunCfg $ fmap counterMax counter ) script
  traverse_ ( executeBuildStep compiler cabal procEnv counter ) steps

-- | Execute a simple 'BuildScript' which does not need any process environment
-- in the 'IO' monad.
executeBuildScriptNoProcessEnv
  :: forall dir
  .  Compiler
  -> Cabal
  -> Maybe Counter
       -- ^ Optional counter to use to report progress.
  -> BuildScript dir ()
       -- ^ The build script to execute.
  -> IO ()
executeBuildScriptNoProcessEnv compiler cabal counter script = do
  let ( _, steps ) = buildSteps ( hostRunCfg $ fmap counterMax counter ) script
      emptyProcEnv :: ProcessEnv dir
      emptyProcEnv =
        ProcessEnv
          { cwd          = sameDirectory
          , extraEnvVars = []
          , extraPATH    = []
          , logBasePath  = Nothing
          }
  traverse_ ( executeBuildStep compiler cabal emptyProcEnv counter ) steps

-- | Execute a single 'BuildStep' in the 'IO' monad.
executeBuildStep :: Compiler
                 -> Cabal
                 -> ProcessEnv dir
                      -- ^ Process environment for the package
                 -> Maybe Counter
                     -- ^ Optional counter to use to report progress.
                 -> BuildStep dir
                 -> IO ()
executeBuildStep compiler cabal procEnv mbCounter = \case
  CallProcess cp  -> callProcessInIO compiler cabal procEnv mbCounter cp
  CreateDir   dir -> createDirectoryIfMissing True ( getAbsolutePath dir )
  LogMessage  msg -> do { putStrLn msg ; hFlush System.stdout }
  ReportProgress { outputProgress } ->
    for_ mbCounter \ counter -> do
      completed <-
        atomicModifyIORef' ( counterRef counter )
          ( \ x -> let !x' = x+1 in (x',x') )
      when outputProgress do
        let
          txt = "## " <> show completed <> " of " <> show ( counterMax counter ) <> " ##"
          n = length txt
        putStrLn $ unlines
          [ ""
          , " " <> replicate n '#'
          , " " <> txt
          , " " <> replicate n '#'
          , "" ]

----------------
-- Script output

-- | Header for shell script output.
shellScriptHeader :: ScriptConfig -> Text
shellScriptHeader scriptCfg =
  Text.unlines $
    [ "#!/bin/bash" , "", "set -ueo pipefail" ] ++
      varsHelper ++ logDir ++ progressVars
  where
    varsHelper
      | Script { useVariables } <- scriptOutput scriptCfg
      , useVariables
      = variablesHelper
      | otherwise
      = []
    progressVars =
      case scriptTotal scriptCfg of
        Nothing -> []
        Just {} ->
          [ "buildEnvProgress=0" ]
    logDir = [ "LOGDIR=\"$PWD/logs/$(date -u +%Y-%m-%d_%H-%M-%S)\""
             , "mkdir -p \"${LOGDIR}\"" ]

-- | Shell script steps for the given 'BuildSteps'.
shellScriptSteps :: forall dir
                 .  Compiler
                 -> Cabal
                 -> ProcessEnv dir
                 -> ScriptConfig
                 -> BuildSteps dir
                 -> Text
shellScriptSteps compiler cabal pkgEnv scriptCfg steps =
  Text.unlines $
    [ "(" ]
    ++ map ( "  " <> )
       ( setProcessEnvSteps scriptType pkgEnv <> mkSteps others )
    ++ [ ")" ]
    ++ mkSteps progress
        -- Take care to update the progress variable outside the subshell.
        -- This assumes progress only happens at the end (which is true currently).

  where
    scriptType =
      case scriptCfg of
        ScriptConfig { scriptOutput = Script { scriptType = ty } } ->
          ty
        _ -> error "shell script steps: not a script"
    mkSteps :: BuildSteps dir -> [ Text ]
    mkSteps = concatMap ( stepScript compiler cabal scriptType ( logBasePath pkgEnv ) ( scriptTotal scriptCfg ) )
    progress, others :: BuildSteps dir
    ( progress, others ) = partition isProgress steps
    isProgress :: BuildStep dir -> Bool
    isProgress ( ReportProgress {} ) = True
    isProgress _ = False


-- | Shell script steps to set the appropriate 'ProcessEnv'.
setProcessEnvSteps :: ScriptType -> ProcessEnv dir -> [ Text ]
setProcessEnvSteps scriptType ( ProcessEnv workDir extraPATH extraEnvVars _logBasePath )
  =  [ "cd " <> q scriptType ExpandVars wd
     | let wd = getSymbolicPath workDir , not ( wd == "." ) ]
  ++ mbUpdatePath
  ++ map mkEnvVar extraEnvVars
  where
    mbUpdatePath :: [ Text ]
    mbUpdatePath
      | null extraPATH
      = [ ]
      | otherwise
      = [  "export PATH=$PATH:"
        <> Text.intercalate ":" ( map Text.pack extraPATH ) -- (already quoted)
        ]

    mkEnvVar :: ( String, String ) -> Text
    mkEnvVar ( var,val ) =
      "export "
      <> Text.pack var
      <> "=" <> Text.pack val -- (already quoted)

-- | A shell script step to call the process specified by the 'CallProcess'
-- argument.
callProcessStep :: Compiler -> Cabal -> ScriptType -> CallProcess dir -> Text
callProcessStep compiler cabal scriptType ( CP { prog, args } ) =
  q scriptType ExpandVars progPath <> " " <> Text.unwords ( map Text.pack args )
  --                        (1)                                    (2)
  --
  -- (1)
  --   In shell scripts, we always change directory before running the
  --   program. As the program path is either absolute or relative to @cwd@,
  --   we don't need to modify the program path.
  --
  -- (2)
  --   Don't quote the arguments: arguments which needed quoting will have
  --   been quoted using the 'quoteArg' function.
  --
  --   This allows users to pass multiple arguments using variables:
  --
  --   > myArgs="arg1 arg2 arg3"
  --   > Setup configure $myArgs
  --
  --   by passing @$myArgs@ as a @Setup configure@ argument.
  where
    progPath = getSymbolicPath $ programPath compiler cabal prog

-- | The underlying shell script of a single build step.
stepScript :: Compiler
           -> Cabal
           -> ScriptType
           -> Maybe ( AbsolutePath File )
               -- ^ Optional logging location
           -> Maybe Word
           -> BuildStep dir
           -> [ Text ]
stepScript compiler cabal scriptType logBasePath totalNbUnits = \case
  CreateDir dir ->
    [ "mkdir -p " <> q scriptType ExpandVars ( getAbsolutePath dir ) ]
  LogMessage str ->
    [ "echo " <> q scriptType ExpandVars str ]
  ReportProgress { outputProgress } ->
    case totalNbUnits of
      Nothing
        -> []
      Just tot
        | outputProgress
        -> [ Text.pack $ "printf \"" <> txt <> "\" $((++buildEnvProgress))" ]
        | otherwise
        -> [ "((++buildEnvProgress))" ]
          -- Still increment the progress variable, as we use this variable
          -- to report progress upon failure.
        where
          n = length $ show tot
          l = 2 * n + 10
          txt = "\\n  " <> replicate l '#'
             <> "\\n  " <> "## %0" <> show n <> "d of " <> show tot <> " ##"
             <> "\\n  " <> replicate l '#' <> "\\n\\n"

  CallProcess cp ->
    -- NB: we ignore the semaphore, as the build scripts we produce
    -- are inherently sequential.
    logCommand ++
    [ cmd <> pipeToLogs
    , resVar <> "=$?"
    , "if [ \"${" <> resVar <> "}\" -eq 0 ]"
    , "then true"
    , "else"
    , "  echo -e " <>
        "\"callProcess failed with non-zero exit code. Command:\\n" <>
        "  > " <> unquote cmd <> "\\n"
      <> logErr <> "\""
    ]
    ++ progressReport
    ++ logMsg
    ++
    [ "  exit \"${" <> resVar <> "}\""
    , "fi" ]
    where
      resVar :: Text
      resVar = "buildEnvLastExitCode"

      cmd :: Text
      cmd = callProcessStep compiler cabal scriptType cp

      logCommand, logMsg :: [Text]
      pipeToLogs, logErr :: Text
      (logCommand, pipeToLogs, logErr, logMsg) =
        case logBasePath of
          Nothing      -> ( [], "", " >&2", [] )
          Just logPath ->
            let stdoutFile, stderrFile :: Text
                stdoutFile = q scriptType ExpandVars ( getAbsolutePath logPath <.> "stdout" )
                stderrFile = q scriptType ExpandVars ( getAbsolutePath logPath <.> "stderr" )
            in ( [ "echo \"> " <> unquote cmd <> "\" >> " <> stdoutFile ]
               , " 2>&1 >" <> stdoutFile <> " | tee -a " <> stderrFile
                  -- Write stdout to the stdout log file.
                  -- Write stderr both to the terminal and to the stderr log file.
               , " | tee -a " <> stderrFile <> " >&2"
               , [ "  echo \"Logs are available at: " <> unquote ( q scriptType ExpandVars ( getAbsolutePath logPath <> ".{stdout,stderr}" ) ) <> "\"" ] )

      progressReport :: [Text]
      progressReport =
        case totalNbUnits of
          Nothing  -> [ ]
          Just tot ->
            [ "  echo \"After ${buildEnvProgress} of " <> Text.pack (show tot) <> "\"" ]

unquote :: Text -> Text
unquote = Text.filter ( not . (== '\"') )

----
-- Helper to check that environment variables are set as expected.

-- | All the environment variables that a shell script using variables
-- expects to be set.
allVars :: [ Text ]
allVars = [ "GHC", "GHCPKG", "SOURCES", "PREFIX", "DESTDIR" ]

-- | A preamble that checks the required environment variables are defined.
variablesHelper :: [ Text ]
variablesHelper =
    [ "", "echo \"Checking that required environment variables are set.\"" ]
    ++ concatMap variableHelper allVars
    ++ [ "" ]

-- | Check that the given environment variable is defined, giving an error
-- message if it isn't (to avoid an error cascade).
variableHelper :: Text -> [ Text ]
variableHelper varName =
  [ "if [ -z ${" <> varName <> "} ]"
  , "then"
  , "  echo \"Environment variable " <> varName <> " not set.\""
  , "  echo \"When using --variables, the build script expects the following environment variables to be set:\""
  , "  echo \"  " <> Text.intercalate ", " allVars <> ".\""
  , "  exit 1"
  , "fi" ]
