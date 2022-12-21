{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

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

    -- ** Shell-script output
  , script

    -- * Build scripts
  , BuildScript, BuildScriptM(..)
  , emptyBuildScript, askScriptConfig
  , buildSteps

    -- ** Individual build steps
  , BuildStep(..), BuildSteps
  , step
  , callProcess, createDir, logMessage

    -- *** Reporting progress
  , Counter(..), reportProgress

    -- ** Configuring build scripts
  , ScriptOutput(..), ScriptConfig(..), hostRunCfg
  , quoteArg

  ) where

-- base
import Data.Foldable
  ( traverse_, foldl', for_ )
import Data.IORef
  ( IORef, atomicModifyIORef' )
import Data.Monoid
  ( Ap(..) )
import Data.String
  ( IsString(..) )

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
  ( Writer, execWriter, tell )

-- build-env
import BuildEnv.Config
  ( Verbosity
  , Style(..), hostStyle
  )
import BuildEnv.Utils
  ( ProgPath(..), CallProcess(..), callProcessInIO )

--------------------------------------------------------------------------------
-- Build scripts: general monad setup.

-- | A build script: a list of build steps, given a 'ScriptConfig' context.
type BuildScript = BuildScriptM ()

deriving via Ap BuildScriptM ()
  instance Semigroup BuildScript
deriving via Ap BuildScriptM ()
  instance Monoid BuildScript

-- | Build script monad.
newtype BuildScriptM a =
  BuildScript
    { runBuildScript :: ReaderT ScriptConfig ( Writer BuildSteps ) a }
  deriving newtype ( Functor, Applicative, Monad )

-- | The empty build script: no build steps.
emptyBuildScript :: BuildScript
emptyBuildScript = return ()

-- | Retrieve the 'ScriptConfig' from the 'ReaderT' environment.
askScriptConfig :: BuildScriptM ScriptConfig
askScriptConfig = BuildScript $ ReaderT return

-- | Obtain the build steps of a 'BuildScript'.
buildSteps :: ScriptConfig -> BuildScript -> BuildSteps
buildSteps cfg buildScript
  = execWriter (runBuildScript buildScript `runReaderT` cfg)

--------------------------------------------------------------------------------
-- Individual build steps

-- | A list of build steps.
type BuildSteps = [BuildStep]

-- | A build step.
data BuildStep
  -- | Call a processs with the given arguments.
  = CallProcess CallProcess
  -- | Create the given directory.
  | CreateDir   FilePath
  -- | Log a message.
  | LogMessage  String
  -- | Report one unit of progress.
  | ReportProgress


-- | A counter to measure progress, as units are compiled.
data Counter =
  Counter
    { counterRef  :: !( IORef Word )
      -- ^ The running count.
    , counterMax :: !Word
      -- ^ The maximum that we're counting up to.
    }

-- | Declare a build step.
step :: BuildStep -> BuildScript
step s = BuildScript $ ReaderT \ _ -> tell [s]

-- | Call a process with given arguments.
callProcess :: CallProcess -> BuildScript
callProcess = step . CallProcess

-- | Create the given directory.
createDir :: FilePath -> BuildScript
createDir = step . CreateDir

-- | Log a message.
logMessage :: Verbosity -> Verbosity -> String -> BuildScript
logMessage v msg_v msg
  | v >= msg_v
  = step $ LogMessage msg
  | otherwise
  = return ()

-- | Report one unit of progress.
reportProgress :: BuildScript
reportProgress = step ReportProgress

--------------------------------------------------------------------------------
-- Configuration

-- | How to interpret the build script: run it in 'IO', or turn it
-- into a shell script?
data ScriptOutput
  -- | Run the build script in 'IO'
  = Run
  -- | Generate a shell script.
  | Shell
    { useVariables :: !Bool
      -- ^ Replace various values with variables, so that
      -- they can be set before running the build script.
      --
      -- Values:
      --
      --  - @GHC@ and @GHC-PKG@,
      --  - fetched sources directory @SOURCES@,
      --  - @PREFIX@ and @DESTDIR@.
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

  , scriptTotal :: !(Maybe Word)
    -- ^ Optional: the total number of units we are building;
    -- used to report progress.
  }

-- | Configure a script to run on the host (in @IO@).
hostRunCfg :: Maybe Word -- ^ Optional: total to report progress against.
           -> ScriptConfig
hostRunCfg mbTotal =
  ScriptConfig
    { scriptOutput = Run
    , scriptStyle  = hostStyle
    , scriptTotal  = mbTotal }

-- | Quote a string, to avoid spaces causing the string
-- to be interpreted as multiple arguments.
q :: ( IsString r, Monoid r ) => String -> r
q t = "\"" <> fromString (escapeArg t) <> "\""
  where
    escapeArg :: String -> String
    escapeArg = reverse . foldl' escape []

    escape :: String -> Char -> String
    escape cs c
      |  '\\' == c
      || '\'' == c
      || '"'  == c
      = c:'\\':cs
      | otherwise
      = c:cs

-- | Quote a command-line argument, if the 'ScriptConfig' requires arguments
-- to be quoted.
--
-- No need to call this on the 'cwd' or 'prog' fields of 'CallProcess',
-- as these will be quoted by the shell-script backend no matter what.
quoteArg :: ( IsString r, Monoid r ) => ScriptConfig -> String -> r
quoteArg ( ScriptConfig { scriptOutput } ) =
  case scriptOutput of
    Run      -> fromString
    Shell {} -> q

--------------------------------------------------------------------------------
-- Interpretation

------
-- IO

-- | Execute a 'BuildScript' in the 'IO' monad.
executeBuildScript :: Maybe Counter -- ^ Optional counter to use to report progress.
                   -> BuildScript   -- ^ The build script to execute.
                   -> IO ()
executeBuildScript counter
  = traverse_  ( executeBuildStep counter )
  . buildSteps ( hostRunCfg $ fmap counterMax counter )

-- | Execute a single 'BuildStep' in the 'IO' monad.
executeBuildStep :: Maybe Counter
                     -- ^ Optional counter to use to report progress.
                 -> BuildStep
                 -> IO ()
executeBuildStep mbCounter = \case
  CallProcess cp  -> callProcessInIO cp
  CreateDir   dir -> createDirectoryIfMissing True dir
  LogMessage  msg -> putStrLn msg
  ReportProgress  -> for_ mbCounter \ counter -> do
    completed <-
      atomicModifyIORef' ( counterRef counter )
        ( \ x -> let !x' = x+1 in (x',x') )
    let
      txt = "## " <> show completed <> " of " <> show ( counterMax counter ) <> " ##"
      n = length txt
    putStrLn $ unlines
      [ ""
      , " " <> replicate n '#'
      , " " <> txt
      , " " <> replicate n '#' ]

----------------
-- Shell script

-- | Obtain the textual contents of a build script.
script :: ScriptConfig -> BuildScript -> Text
script scriptCfg buildScript =
  Text.unlines ( header ++ concatMap ( stepScript scriptCfg ) ( buildSteps scriptCfg buildScript ) )
  where
    header, varsHelper, progressVars :: [ Text ]
    header = [ "#!/bin/bash" , "" ] ++ varsHelper ++ progressVars
    varsHelper
      | Shell { useVariables } <- scriptOutput scriptCfg
      , useVariables
      = variablesHelper
      | otherwise
      = []
    progressVars =
      case scriptTotal scriptCfg of
        Nothing -> []
        Just {} ->
          [ "buildEnvProgress=0" ]

-- | The underlying script of a build step.
stepScript :: ScriptConfig -> BuildStep -> [ Text ]
stepScript scriptCfg = \case
  CreateDir dir ->
    [ "mkdir -p " <> q dir ]
  LogMessage str ->
    [ "echo " <> q str ]
  ReportProgress ->
    case scriptTotal scriptCfg of
      Nothing  -> []
      Just tot ->
        [ Text.pack $ "printf \"" <> txt <> "\" $((++buildEnvProgress))" ]
        where
          n = min 1 (length $ show tot)
          l = 2 * n + 10
          txt = "\\n " <> replicate l '#' <> "\\n "
                      <> "## %0" <> show n <> "d of " <> show tot <> " ##" <> "\\n "
                      <> replicate l '#' <> "\\n"

  CallProcess ( CP { cwd, extraPATH, extraEnvVars, prog, args } ) ->
    -- NB: we ignore the semaphore, as the build scripts we produce
    -- are inherently sequential.
    [ "( cd " <> q cwd <> " ; \\" ]
    ++ mbUpdatePath
    ++ map mkEnvVar extraEnvVars
    ++
    [ "  " <> cmd <> " )"
    , resVar <> "=$?"
    , "if [ \"${" <> resVar <> "}\" -eq 0 ]"
    , "then true"
    , "else"
    , "  echo \"callProcess failed with non-zero exit code. Command:\""
    , "  echo " <> cmd
    , "  exit \"${" <> resVar <> "}\""
    , "fi" ]
    where
      cmd :: Text
      cmd = q ( progPath prog ) <> " " <> Text.unwords (map Text.pack args)
        --         (1)                                         (2)
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
      resVar :: Text
      resVar = "buildEnvLastExitCode"
      mbUpdatePath :: [Text]
      mbUpdatePath
        | null extraPATH
        = []
        | otherwise
        = [  "  export PATH=$PATH:"
          <> Text.intercalate ":" (map Text.pack extraPATH) -- (already quoted)
          <> " ; \\" ]

      mkEnvVar :: (String, String) -> Text
      mkEnvVar (var,val) = "  export "
                        <> Text.pack var
                        <> "=" <> Text.pack val <> " ; \\"
                                   -- (already quoted)

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
