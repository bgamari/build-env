module Main where

-- base
import Control.Monad
  ( when )

-- bytestring
import qualified Data.ByteString.Lazy as BSL
  ( readFile, writeFile )

-- build-env
import Build
  ( buildPlan, computePlan, fetchPlan, parsePlanBinary )
import CabalPlan
  ( CabalPlan, CabalPlanBinary(..) )
import Config
  ( Cabal )
import Options
import Parse
  ( runOptionsParser )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  Opts { compiler, cabal, mode, verbosity } <- runOptionsParser
  case mode of
    PlanMode { planModeInputs, planOutput } -> do
      CabalPlanBinary planBinary <-
        computePlanFromInputs verbosity cabal planModeInputs
      verboseMsg verbosity $ "Writing build plan to " ++ planOutput
      BSL.writeFile planOutput planBinary
    FetchMode ( FetchInputs { fetchDir, fetchInputPlan } ) -> do
      plan <- getPlan verbosity cabal fetchInputPlan
      doFetch verbosity cabal fetchDir plan
    BuildMode ( Build { buildFetchInputs = FetchInputs { fetchDir, fetchInputPlan }
                      , buildFetch, buildStrategy, buildOutputDir } ) -> do
      plan <- getPlan verbosity cabal fetchInputPlan
      case buildFetch of
        Prefetched -> return ()
        Fetch      -> doFetch verbosity cabal fetchDir plan
      verboseMsg verbosity "Building and registering packages"
      buildPlan compiler buildStrategy fetchDir buildOutputDir plan

computePlanFromInputs :: Verbosity -> Cabal -> PlanInputs -> IO CabalPlanBinary
computePlanFromInputs verbosity cabal
  (PlanInputs { planPins, planPkgs, planAllowNewer })
    = do verboseMsg verbosity "Computing build plan"
         computePlan cabal planAllowNewer planPins planPkgs

getPlan :: Verbosity -> Cabal -> Plan -> IO CabalPlan
getPlan verbosity cabal planMode = do
   planBinary <-
     case planMode of
       ComputePlan planInputs   ->
        computePlanFromInputs verbosity cabal planInputs
       UsePlan { planJSONPath } ->
         do
           verboseMsg verbosity $ "Reading build plan from " ++ planJSONPath
           CabalPlanBinary <$> BSL.readFile planJSONPath
   return $ parsePlanBinary planBinary

doFetch :: Verbosity -> Cabal -> FilePath -> CabalPlan -> IO ()
doFetch verbosity cabal fetchDir plan = do
  verboseMsg verbosity "Fetching sources from build plan"
  fetchPlan cabal fetchDir plan

verboseMsg :: Verbosity -> String -> IO ()
verboseMsg v msg = when (v >= Verbosity 1) $ putStrLn msg
