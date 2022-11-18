{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Build
    ( PkgSpec(..)
    , computePlan
    , buildPlan
    , test
    ) where

import Control.Applicative
import Control.Monad.Fix
import Data.List
import Data.Maybe
import Data.Version
import qualified Data.Graph as Graph
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Control.Concurrent.Async

import BuildOne
import CabalPlan
import Utils

data Constraint = Constraint

data PkgSpec = PkgSpec { psName :: PkgName
                       , psConstraints :: Maybe Constraint
                       , psFlags :: FlagSpec
                       }

cabalPath = "cabal"

computePlan :: [PkgSpec] -> IO CabalPlan
computePlan pkgs = withTempDir "build" $ \dir -> do
    writeFile (dir </> "cabal.project") projectContents
    writeFile (dir </> "dummy-package.cabal") cabalContents
    callProcessIn dir cabalPath ["configure", "-v0"]
    mb_plan <- Aeson.eitherDecode <$> BSL.readFile (dir </> "dist-newstyle" </> "cache" </> "plan.json")
    case mb_plan of
      Left err -> fail err
      Right plan -> return plan
  where
    projectContents = unlines
        [ "packages: ."
        ] ++
        unlines
        [ unlines
          [ "package " <> T.unpack (unPkgName $ psName ps)
          , "  flags: " <> showFlagSpec (psFlags ps)
          ]
        | ps <- pkgs
        ]

    cabalContents = unlines
        [ "cabal-version: 2.4"
        , "name: dummy-package"
        , "version: 0"
        , "library"
        , "  build-depends:"
        ] ++ intercalate "," [ "    " <> T.unpack (unPkgName $ psName ps) | ps <- pkgs ]

sortPlan :: CabalPlan -> [PlanUnit]
sortPlan plan =
    map (fst3 . lookupVertex) $ Graph.reverseTopSort gr
  where
    fst3 (x,_,_) = x
    (gr, lookupVertex) = Graph.graphFromEdges'
       [ (x, puId x, allDepends x)
       | x <- planUnits plan
       ]

allDepends :: PlanUnit -> [PkgId]
allDepends (PreexistingUnit{puDepends}) = puDepends
allDepends (ConfiguredUnit{puDepends, puSetupDepends}) = puDepends ++ puSetupDepends

buildPlan :: FilePath -> Compiler -> CabalPlan -> IO ()
buildPlan installDir0 comp cabalPlan = do
    installDir <- canonicalizePath installDir0
    let sorted = filter (\pu -> puId pu /= PkgId "dummy-package-0-inplace") $ sortPlan cabalPlan
    let doPkg :: PlanUnit -> IO ()
        doPkg (PreexistingUnit{}) = return ()
        doPkg pu@(ConfiguredUnit{}) = withTempDir "source" $ \dir -> do
            putStrLn $ "building " ++ show (puId pu)
            srcDir <- cabalFetch dir (puPkgName pu) (puVersion pu)
            buildPackage srcDir (puComponentName pu) installDir comp (puFlags pu)

    let unitMap :: ML.Map PkgId PlanUnit
        unitMap = ML.fromList [ (puId pu, pu) | pu <- sorted ]
    unitAsyncs <- mfix $ \unitAsyncs ->
        let doPkgAsync :: PlanUnit -> IO ()
            doPkgAsync pu = do
                mapM_ (wait . (unitAsyncs M.!)) (allDepends pu)
                doPkg pu
         in traverse (async . doPkgAsync) unitMap 
    mapM_ wait unitAsyncs

cabalFetch :: FilePath -> PkgName -> Version -> IO FilePath
cabalFetch root pkgName version = do
    callProcessIn root cabalPath [ "unpack", pkgNameVersion pkgName version ]
    return (root </> pkgNameVersion pkgName version)

pkgNameVersion :: PkgName -> Version -> String
pkgNameVersion (PkgName n) v = T.unpack n <> "-" <> showVersion v

test :: IO ()
test = do
    plan <- computePlan
        [ PkgSpec { psName = PkgName "lens", psConstraints = Nothing, psFlags = mempty }
        ]
    print plan
    let comp = Compiler "ghc" "ghc-pkg"
        installDir = "install"
    buildPlan installDir comp plan

