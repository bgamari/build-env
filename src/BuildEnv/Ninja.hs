{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  BuildEnv.Ninja
-- Description :  Ninja file output
--
-- This modules provides ninja file support for @build-env@
module BuildEnv.Ninja where

-- base
import Data.Either
  ( partitionEithers )
import Data.Maybe
  ( catMaybes, fromMaybe )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text

-- build-env
import BuildEnv.CabalPlan
import BuildEnv.Config
import BuildEnv.Path
import BuildEnv.Script
import BuildEnv.Utils

--------------------------------------------------------------------------------

ninja :: Compiler
      -> Style
      -> Map UnitId PlanUnit
         -- ^ All units in the build plan, including pre-existing units
      -> BuildPaths ForBuild
      -> BuildSteps prepDir
          -- ^ Preparatory steps
      -> BuildSteps endDir
          -- ^ Final steps
      -> [ ( ConfiguredUnit
           , Either UnitId ( ProcessEnv Pkg, BuildSteps Pkg )
           , ( ProcessEnv Pkg, BuildSteps Pkg )
           ) ]
          -- ^ Plan to build
      -> Text
ninja compiler scriptStyle allUnits ( BuildPaths { prefix } ) prep finish planUnits =
  Text.unlines $ prepRules <> setupRules <> buildRules <> regRules <> finishRules

  where
    setups :: [ ( ConfiguredUnit, ( ProcessEnv Pkg, Set UnitId, BuildSteps Pkg ) ) ]
    setups =
      [ ( cu, ( env, configuredDeps $ Set.fromList sDeps, s ) )
      | ( cu@( ConfiguredUnit { puSetupDepends = sDeps } ), Right ( env, s ), _ ) <- planUnits ]

    builds :: [ ( ConfiguredUnit, ( ProcessEnv Pkg, UnitId, Set UnitId, Set UnitId, BuildSteps Pkg ) ) ]
    builds =
      [ ( cu, ( env, case mbSetup of { Left i -> i; Right {} -> puId }, configuredDeps ( Set.fromList exeDeps ), configuredDeps ( Set.fromList libDeps ), b ) )
      | ( cu@( ConfiguredUnit { puId, puExeDepends = exeDeps, puDepends = libDeps } ), mbSetup, ( env, b ) ) <- planUnits ]

    configuredDeps :: Set UnitId -> Set UnitId
    configuredDeps = Set.filter isConfigured

    isConfigured :: UnitId -> Bool
    isConfigured uid = case Map.lookup uid allUnits of
      Just u ->
        case u of
          PU_Configured  {} -> True
          PU_Preexisting {} -> False
      Nothing ->
        error $ "ninja: could not look up " <> show uid

    cmdLine :: Maybe ( ProcessEnv Pkg ) -> BuildSteps dir -> Text
    cmdLine mbProcEnv steps
      | null cps && null dirs
      = ":"
      | null cps
      = makeCommand $ env <> "mkdir -p " <> Text.unwords ( map Text.pack dirs )
      | null dirs
      = makeCommand $ env <> Text.intercalate " && " ( map ( callProcessStep compiler cabal Ninja ) cps )
      | otherwise
      = "internal error in ninja backend: mix of CreateDir and CallProcess"

        where
          makeCommand cmd = case scriptStyle of
            WinStyle   -> "cmd /c \"" <> Text.concatMap ( \ case { '\"' -> "^\""; c -> Text.singleton c } ) cmd <> "\""
            PosixStyle -> cmd

          emptyProcEnv :: ProcessEnv Pkg
          emptyProcEnv =
            ProcessEnv
              { cwd          = sameDirectory
              , extraPATH    = []
              , extraEnvVars = []
              , logBasePath  = Nothing
              }
          env = mconcat $ map ( <> " && " ) $ setProcessEnvSteps Ninja ( fromMaybe emptyProcEnv mbProcEnv )

          ( dirs, cps ) = partitionEithers $ catMaybes $ map f steps

          f :: BuildStep dir -> Maybe ( Either FilePath ( CallProcess dir ) )
          f ( CreateDir dir )     = Just ( Left $ getAbsolutePath dir )
          f ( CallProcess cp )    = Just ( Right cp )
          f ( ReportProgress {} ) = Nothing
          f ( LogMessage {} )     = Nothing

    cabal :: Cabal
    cabal = error "cabal-install not needed for Ninja"

    prepRules, setupRules, buildRules, regRules, finishRules :: [ Text ]
    prepRules =
      [ "rule rule_prep"
      , "  description = Create package database directory"
      , "  command = " <> cmdLine Nothing prep
      , "build prep: rule_prep"
      , ""
      , "pool package_db_lock"
      , "   depth = 1"
      , ""
      , "prefix = " <> q Ninja ExpandVars ( getAbsolutePath prefix )
      ]
    finishRules =
      [ "rule rule_finish"
      , "  command = " <> cmdLine Nothing finish
      ] ++
      [ "build finish: rule_finish |"
        <> mconcat [ " " <> what <> "_" <> uid
                   | ( ConfiguredUnit { puId = UnitId uid }, ( _, _, _, _, steps ) ) <- builds
                   , let what = if any registerStep steps
                                then "register"
                                else "build"
                   ]
      , "default finish"
      , "" ]
    setupRules =
      [ Text.unlines $
          [ "rule rule_setup_" <> uid
          , "  description = Setup " <> pkgNameVersion pkg ver
          , "  command = " <> cmdLine ( Just procEnv ) steps
          ] ++
          [ "build setup_" <> uid <> ": rule_setup_" <> uid
          <> " | prep" <> mconcat [ " setup_" <> sId | UnitId sId <- Set.toList sDeps ]
          ]
      | ( ConfiguredUnit { puId = UnitId uid, puPkgName = pkg, puVersion = ver }, ( procEnv, sDeps, steps ) ) <- setups
      ]
    buildRules =
      [ Text.unlines $
          [ "rule rule_build_" <> uid
          , "  description = Build " <> cabalComponent ( puComponentName cu ) <> " from " <> pkgNameVersion pkg ver
          , "  command = " <> cmdLine ( Just procEnv ) ( filter ( not . registerStep ) steps )
          ] ++
          [ "build build_" <> uid <> ": rule_build_" <> uid
          <> " | " <> " setup_" <> setupDep
                   <> mconcat [ " build_" <> dId | UnitId dId <- Set.toList exeDeps ]
                   <> mconcat [ " register_" <> dId | UnitId dId <- Set.toList libDeps ]
          ]
      | ( cu@( ConfiguredUnit { puId = UnitId uid, puPkgName = pkg, puVersion = ver } ), ( procEnv, UnitId setupDep, exeDeps, libDeps, steps ) ) <- builds
      ]

    regRules =
      [ Text.unlines $
          [ "rule rule_register_" <> uid
          , "  description = Register " <> cabalComponent ( puComponentName cu ) <> " from " <> pkgNameVersion pkg ver
          , "  command = " <> cmdLine ( Just basicProcEnv ) ( filter registerStep steps )
          , "  pool = package_db_lock"
             -- NB: this prevents concurrent modifications of the package database.
          ] ++
          [ "build register_" <> uid <> ": rule_register_" <> uid
          <> " | build_" <> uid
          ]
      | ( cu@( ConfiguredUnit { puId = UnitId uid, puPkgName = pkg, puVersion = ver } ), ( procEnv, _, _, _, steps ) ) <- builds
      , let basicProcEnv = procEnv { extraEnvVars = [], extraPATH = [] }
          -- The environment variables aren't needed for the ghc-pkg invocation,
          -- so we might as well omit them to simplify the Ninja output.
      ]

    registerStep :: BuildStep Pkg -> Bool
    registerStep ( CallProcess ( CP { prog = GhcPkgProgram } ) ) = True
    registerStep _ = False
