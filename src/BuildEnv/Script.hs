
-- |
-- Module      :  BuildEnv.Script
-- Description :  Tiny build script DSL
--
-- This modules provides a tiny build script DSL.
--
-- A 'BuildScript' is a series of simple build steps (process calls).
--
-- A 'BuildScript' can be executed in the 'IO' monad, using 'runBuildScript'.
--
-- A 'BuildScript' can be turned into a shell script which can be executed
-- later, using 'script'.
module BuildEnv.Script
  ( -- * Build scripts
    BuildScript
  , runBuildScript, script

    -- * Individual build steps
  , BuildStep(..)
  , callProcess, logMessage, step
  ) where

-- base
import Data.Foldable
  ( traverse_, foldl' )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text

-- transformers
import Control.Monad.Trans.Writer.CPS
  ( Writer, tell )

-- build-env
import BuildEnv.Config
  ( Verbosity )
import BuildEnv.Utils
  ( CallProcess(..), callProcessInIO )

--------------------------------------------------------------------------------

-- | A build script: a list of build steps.
type BuildScript = [BuildStep]

-- | A build step.
data BuildStep
  -- | Call a processs with the given arguments.
  = CallProcess CallProcess
  -- | Log a message.
  | LogMessage String

-- | Execute a build script in the 'IO' monad.
runBuildScript :: BuildScript -> IO ()
runBuildScript = traverse_ runBuildStep

-- | Execute a single build step in the 'IO' monad.
runBuildStep :: BuildStep -> IO ()
runBuildStep (CallProcess cp ) = callProcessInIO cp
runBuildStep (LogMessage  msg) = putStrLn msg

-- | Obtain the textual contents of a build script.
script :: BuildScript -> Text
script steps =
  Text.unlines ( header ++ concatMap stepScript steps )
  where
    header :: [ Text ]
    header = [ "#/bin/bash" , "" ]

-- | Declare a build step.
step :: BuildStep -> Writer BuildScript ()
step = tell . (:[])

-- | Call a process with given arguments.
callProcess :: CallProcess -> Writer BuildScript ()
callProcess = step . CallProcess

-- | Log a message.
logMessage :: Verbosity -> Verbosity -> String -> Writer BuildScript ()
logMessage v msg_v msg
  | v >= msg_v
  = step $ LogMessage msg
  | otherwise
  = return ()

-- | The underlying script of a build step.
stepScript :: BuildStep -> [ Text ]
stepScript ( CallProcess ( CP { cwd, extraPATH, extraEnvVars, prog, args } ) ) =
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
    , "  echo \"> " <> pprCmd <> " \""
    , "  exit \"${" <> resVar <> "}\""
    , "fi" ]
  where
    cmd :: Text
    cmd = q prog <> " " <> Text.unwords (map q args)
    pprCmd :: Text
    pprCmd = Text.pack prog <> " " <> Text.unwords (map Text.pack args)
    resVar :: Text
    resVar = "lastExitCode"
    mbUpdatePath :: [Text]
    mbUpdatePath
      | null extraPATH
      = []
      | otherwise
      = [  "  export PATH=$PATH:"
        <> Text.intercalate ":" (map q extraPATH)
        <> " ; \\" ]


    mkEnvVar :: (String, String) -> Text
    mkEnvVar (var,val) = "  export "
                      <> Text.pack var
                      <> "=" <> q val <> " ; \\"
stepScript (LogMessage str) =
  [ "echo " <> q str ]


q :: String -> Text
q t = "\"" <> Text.pack (escapeArg t) <> "\""
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
