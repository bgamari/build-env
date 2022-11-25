{-# LANGUAGE OverloadedStrings #-}

module Build
    ( computePlan
    , parsePlanBinary
    , fetchPlan
    , buildPlan
    , test
    ) where

-- base
import Control.Monad.Fix
  ( MonadFix(mfix) )
import Data.Char
  ( isSpace )
import Data.Foldable
  ( for_ )
import Data.List
  ( intercalate, nub )
import Data.Maybe
  ( mapMaybe, maybeToList )
import Data.Version
  ( Version, showVersion )

-- aeson
import qualified Data.Aeson as Aeson
  ( eitherDecode )

-- async
import Control.Concurrent.Async
  ( async, wait )

-- bytestring
import qualified Data.ByteString.Lazy as Lazy.ByteString
  ( readFile )

-- containers
import qualified Data.Graph as Graph
  ( graphFromEdges', reverseTopSort )
import qualified Data.Map.Strict as Map
  ( (!?), assocs, empty, fromList, keys, union )
import qualified Data.Map.Lazy as Lazy
  ( Map )
import qualified Data.Map.Lazy as Lazy.Map
  ( fromList )

-- directory
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing )

-- filepath
import System.FilePath
  ( (</>) )

-- text
import qualified Data.Text as Text
  ( all, unpack )

-- build-env
import BuildOne
  ( buildPackage )
import CabalPlan
import Config
import Utils
  ( callProcessIn, withTempDir )

--------------------------------------------------------------------------------

computePlan :: Cabal
            -> AllowNewer
            -> PkgSpecs -- ^ pins: packages which we want to constrain at
                        -- a particular version/flag, but which we don't
                        -- necessarily want to build
            -> PkgSpecs -- ^ packages we want to build
                        -- (these override the pins)
            -> IO CabalPlanBinary
computePlan cabal (AllowNewer allowNewer) pins pkgs =
  withTempDir "build" \ dir -> do
    putStrLn $ "Computing plan in build directory " ++ dir
    writeFile (dir </> "cabal.project") projectContents
    writeFile (dir </> "dummy-package.cabal") cabalContents
    callProcessIn dir (cabalPath cabal) ["build", "--dry-run", "-v0"]
    let planPath = dir </> "dist-newstyle" </> "cache" </> "plan.json"
    CabalPlanBinary <$> Lazy.ByteString.readFile planPath
  where
    allPkgs = pkgs `Map.union` pins -- union is left-biased: override pins with pkgs
    projectContents = unlines
        [ "packages: ."
        ] ++ allowNewers
          ++ flagSpecs
        ++ constraints

    constraints = unlines
        [ unwords ["constraints:", Text.unpack (unPkgName nm), Text.unpack cts]
        | (nm, ps) <- Map.assocs allPkgs
        , Constraints cts <- maybeToList $ psConstraints ps
        , not (Text.all isSpace cts)
        ]

    allowNewers
      | null allowNewer
      = ""
      | otherwise
      = "\nallow-newer:\n" ++
        intercalate ","
          [ "    " <> Text.unpack p <> ":" <> Text.unpack q <> "\n"
          | (p,q) <- allowNewer ]

    flagSpecs = unlines
        [ unlines
          [ "package " <> Text.unpack (unPkgName nm)
          , "  flags: " <> showFlagSpec (psFlags ps)
          ]
        | (nm, ps) <- Map.assocs allPkgs
        , let flags = psFlags ps
        , not $ flagSpecIsEmpty flags
        ]

    cabalContents = unlines
        [ "cabal-version: 2.4"
        , "name: dummy-package"
        , "version: 0"
        , "library"
        , "  build-depends:"
        ] ++ intercalate ",\n"
             [ "    " <> Text.unpack (unPkgName nm)
             | nm <- Map.keys pkgs ]

parsePlanBinary :: CabalPlanBinary -> CabalPlan
parsePlanBinary (CabalPlanBinary pb) =
  case Aeson.eitherDecode pb of
    Left  err  -> error ("parsePlan: failed to parse plan JSON\n" ++ err)
    Right plan -> plan

allDepends :: ConfiguredUnit -> [PkgId]
allDepends (ConfiguredUnit{puDepends, puSetupDepends}) = puDepends ++ puSetupDepends

fetchPlan :: Cabal -> FilePath -> CabalPlan -> IO ()
fetchPlan cabal fetchDir0 cabalPlan = do
    fetchDir <- canonicalizePath fetchDir0
    createDirectoryIfMissing True fetchDir
      -- TODO: cabal will fail if the src directories already exist
    mapM_ (uncurry $ cabalFetch cabal fetchDir) pkgs
  where
    pkgs = nub -- Some packages might have multiple components; we don't want to fetch the package multiple times.
         $ mapMaybe (\case { PU_Configured ( ConfiguredUnit { puId = pkgId, puPkgName = nm, puVersion = ver } )
                              | pkgId /= PkgId "dummy-package-0-inplace"
                              -> Just (nm, ver)
                           ; _ -> Nothing })
         $ planUnits cabalPlan

cabalFetch :: Cabal -> FilePath -> PkgName -> Version -> IO ()
cabalFetch cabal root nm ver = do
    callProcessIn root (cabalPath cabal)
      [ "unpack", pkgNameVersion nm ver ]

pkgNameVersion :: PkgName -> Version -> String
pkgNameVersion (PkgName n) v = Text.unpack n <> "-" <> showVersion v

sortPlan :: CabalPlan -> [ConfiguredUnit]
sortPlan plan =
    map (fst3 . lookupVertex) $ Graph.reverseTopSort gr
  where
    fst3 :: (a,b,c) -> a
    fst3 (a,_,_) = a
    (gr, lookupVertex) = Graph.graphFromEdges'
       [ (pu, puId, allDepends pu)
       | PU_Configured pu@(ConfiguredUnit { puId }) <- planUnits plan
       ]

buildPlan :: Compiler -> BuildStrategy -> FilePath -> FilePath -> CabalPlan -> IO ()
buildPlan comp buildStrat fetchDir0 installDir0 cabalPlan = do
    fetchDir <- canonicalizePath fetchDir0
    installDir <- canonicalizePath installDir0
    createDirectoryIfMissing True installDir
    let buildPkg :: ConfiguredUnit -> IO ()
        buildPkg pu@(ConfiguredUnit { puPkgName, puVersion }) = do
            let srcDir = fetchDir </> pkgNameVersion puPkgName puVersion
            buildPackage comp srcDir installDir cabalPlan pu

    if doAsync buildStrat
    then do unitAsyncs <- mfix \ unitAsyncs ->
              let doPkgAsync :: ConfiguredUnit -> IO ()
                  doPkgAsync pu = do
                      for_ (allDepends pu) \ depPkgId ->
                        for_ (unitAsyncs Map.!? depPkgId) \ cu ->
                          -- (Nothing for Preexisting packages)
                          wait cu
                      buildPkg pu
               in traverse (async . doPkgAsync) unitMap
            mapM_ wait unitAsyncs
    else for_ unitsToBuild buildPkg

  where

    unitsToBuild :: [ConfiguredUnit]
    unitsToBuild
      = filter (\ ( ConfiguredUnit { puId } ) -> puId /= PkgId "dummy-package-0-inplace")
      $ if doTopoSort buildStrat
        then sortPlan cabalPlan
        else mapMaybe configuredUnitMaybe $ planUnits cabalPlan

    unitMap :: Lazy.Map PkgId ConfiguredUnit
    unitMap = Lazy.Map.fromList
              [ (puId, pu)
              | pu@( ConfiguredUnit { puId } ) <- unitsToBuild ]

test :: IO ()
test = do
    let cabal      = Cabal "cabal"
        compiler   = Compiler "ghc" "ghc-pkg"
        fetchDir   = "fetch"
        installDir = "install"
        pins, pkgs :: PkgSpecs
        pins = Map.empty
        pkgs = Map.fromList
             [ ( PkgName "lens"
               , PkgSpec { psConstraints = Nothing, psFlags = mempty } )
             , ( PkgName "finitary"
               , PkgSpec { psConstraints = Just (Constraints ">= 2.1 && < 3.0")
                         , psFlags = FlagSpec $ Map.fromList [ ("bitvec", False), ("vector", True) ] } )
             , ( PkgName "gi-atk"
               , PkgSpec { psConstraints = Just (Constraints ">= 2.0.25"), psFlags = mempty } )
               ] -- Agda 2.6.2.2
    pb <- computePlan cabal (AllowNewer [("*", "base")]) pins pkgs
    let plan = parsePlanBinary pb
    print plan
    --fetchPlan cabal fetchDir plan
    buildPlan compiler TopoSort fetchDir installDir plan
