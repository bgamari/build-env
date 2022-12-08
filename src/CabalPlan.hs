{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  CabalPlan
-- Description :  Parsing @cabal@ @plan.json@ files.
module CabalPlan where

-- base
import Data.Char
  ( isAlphaNum )
import Data.Maybe
  ( fromMaybe, mapMaybe )
import Data.Version
  ( Version, showVersion )

-- aeson
import Data.Aeson

-- bytestring
import qualified Data.ByteString.Lazy as Lazy
  ( ByteString )

-- containers
import qualified Data.Map.Strict as Strict
  ( Map )
import qualified Data.Map.Strict as Map
import Data.Set
  ( Set )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text

-------------------------------------------------------------------------------

-- | Units in a @cabal@ @plan.json@ file.
data CabalPlan = CabalPlan { planUnits :: [PlanUnit] }

mapMaybePlanUnits :: (PlanUnit -> Maybe a) -> CabalPlan -> [a]
mapMaybePlanUnits f (CabalPlan units) = mapMaybe f units

instance Show CabalPlan where
  show (CabalPlan us)
    = unlines $ map show us

instance FromJSON CabalPlan where
    parseJSON = withObject "cabal plan" \ o ->
        CabalPlan <$> o .: "install-plan"

-- | A unique identifier for a package,
-- e.g. @lens-5.2-1bfd85cb66d2330e59a2f957e87cac993d922401@
newtype UnitId = UnitId { unUnitId :: Text }
    deriving stock Show
    deriving newtype (Eq, Ord, FromJSON, FromJSONKey)

-- | A cabal package name, e.g. @lens@, @aeson@.
newtype PkgName = PkgName { unPkgName :: Text }
    deriving stock   Show
    deriving newtype (Eq, Ord, FromJSON, FromJSONKey)

-- | The name + version string of a package.
pkgNameVersion :: PkgName -> Version -> Text
pkgNameVersion (PkgName n) v = n <> "-" <> Text.pack (showVersion v)

-- | Is the string a valid @cabal@ package name? That is, does it consist
-- only of alphanumeric identifiers and hyphens?
validPackageName :: Text -> Bool
validPackageName = Text.all ( \ x -> isAlphaNum x || x == '-' )

-- | Specification of package flags, e.g. @+foo -bar@.
--
-- @+@ corresponds to @True@ and @-@ to @False@.
newtype FlagSpec = FlagSpec (Strict.Map Text Bool)
    deriving stock   Show
    deriving newtype (Eq, Ord, Semigroup, Monoid, FromJSON)

showFlagSpec :: FlagSpec -> Text
showFlagSpec (FlagSpec fs) =
    Text.unwords
    [ sign <> flag
    | (flag, value) <- Map.toList fs
    , let sign = if value then "+" else "-"
    ]

flagSpecIsEmpty :: FlagSpec -> Bool
flagSpecIsEmpty (FlagSpec fs) = null fs

-- | The type of a component, e.g. library, executable, test-suite...
data ComponentType
  = Lib
  | FLib
  | Exe
  | Test
  | Bench
  | Setup
  deriving stock (Eq, Ord, Show)

-- | Parse the type of a @cabal@ component, e.g library, executable, etc.
parseComponentType :: Text -> Maybe ComponentType
parseComponentType "lib"   = Just Lib
parseComponentType "flib"  = Just FLib
parseComponentType "exe"   = Just Exe
parseComponentType "test"  = Just Test
parseComponentType "bench" = Just Bench
parseComponentType "setup" = Just Setup
parseComponentType _       = Nothing

-- | Print the cabal component type as expected in cabal colon syntax
-- @pkg:ty:comp@.
cabalComponentType :: ComponentType -> Text
cabalComponentType Lib   = "lib"
cabalComponentType FLib  = "flib"
cabalComponentType Exe   = "exe"
cabalComponentType Test  = "test"
cabalComponentType Bench = "bench"
cabalComponentType Setup = "setup"

-- | The name of a cabal component, e.g. @lib:comp@.
data ComponentName =
  ComponentName { componentType :: ComponentType
                   -- ^ What's before the colon, e.g. @lib@, @exe@, @setup@...
                , componentName :: Text
                   -- ^ The actual name of the component
                }
    deriving stock (Eq, Ord, Show)

-- | Print a cabal component using colon syntax @ty:comp@.
cabalComponent :: ComponentName -> Text
cabalComponent (ComponentName ty nm) = cabalComponentType ty <> ":" <> nm

-- | Parse a cabal package component, using the syntax @pkg:ty:comp@,
-- e.g. @attoparsec:lib:attoparsec-internal@.
parsePkgComponent :: Text -> Maybe ( PkgName, ComponentName )
parsePkgComponent txt = case Text.splitOn ":" txt of
  ty:pkg:[]
    | Just t <- parseComponentType ty
    , validPackageName pkg
    -> Just ( PkgName pkg, ComponentName t pkg )
  pkg:ty:comp:[]
    | Nothing <- parseComponentType pkg
    , validPackageName pkg
    , Just t <- parseComponentType ty
    , validPackageName comp
    -> Just ( PkgName pkg, ComponentName t comp )
  pkg:comp:[]
    | Nothing <- parseComponentType pkg
    , validPackageName pkg
    , validPackageName comp
    -> Just ( PkgName comp, ComponentName Lib comp )
  pkg:[]
    | Nothing <- parseComponentType pkg
    , validPackageName pkg
    -> Just ( PkgName pkg, ComponentName Lib pkg )
  _ -> Nothing


data PlanUnit
  = PU_Preexisting PreexistingUnit
  | PU_Configured  ConfiguredUnit
  deriving stock Show

configuredUnitMaybe :: PlanUnit -> Maybe ConfiguredUnit
configuredUnitMaybe (PU_Configured pu)  = Just pu
configuredUnitMaybe (PU_Preexisting {}) = Nothing

planUnitUnitId :: PlanUnit -> UnitId
planUnitUnitId (PU_Preexisting (PreexistingUnit { puId })) = puId
planUnitUnitId (PU_Configured  (ConfiguredUnit  { puId })) = puId

planUnitPkgName :: PlanUnit -> PkgName
planUnitPkgName (PU_Preexisting (PreexistingUnit { puPkgName })) = puPkgName
planUnitPkgName (PU_Configured  (ConfiguredUnit  { puPkgName })) = puPkgName

planUnitVersion :: PlanUnit -> Version
planUnitVersion (PU_Preexisting (PreexistingUnit { puVersion })) = puVersion
planUnitVersion (PU_Configured  (ConfiguredUnit  { puVersion })) = puVersion

-- | All the dependencies of a unit: @depends@, @exe-depends@ and @setup-depends@.
allDepends :: ConfiguredUnit -> [UnitId]
allDepends (ConfiguredUnit { puDepends, puExeDepends, puSetupDepends }) =
  puDepends ++ puExeDepends ++ puSetupDepends

-- | Information about a built-in pre-existing unit (such as @base@).
data PreexistingUnit
  = PreexistingUnit
    { puId      :: UnitId
    , puPkgName :: PkgName
    , puVersion :: Version
    , puDepends :: [UnitId]
    }
  deriving stock Show

-- | Information about a unit: name, version, dependencies, flags.
data ConfiguredUnit
  = ConfiguredUnit
    { puId            :: UnitId
    , puPkgName       :: PkgName
    , puVersion       :: Version
    , puComponentName :: ComponentName
    , puFlags         :: FlagSpec
    , puDepends       :: [UnitId]
    , puExeDepends    :: [UnitId]
    , puSetupDepends  :: [UnitId]
    }
  deriving stock Show

-- | Get what kind of component this unit is: @lib@, @exe@, etc.
cuComponentType :: ConfiguredUnit -> ComponentType
cuComponentType = componentType . puComponentName

-- | A @cabal@ mangled package name, in which @-@ has been replaced with @_@.
mangledPkgName :: PkgName -> String
mangledPkgName = map fixupChar . Text.unpack . unPkgName
  where
    fixupChar '-' = '_'
    fixupChar c   = c

instance FromJSON PlanUnit where
    parseJSON = withObject "plan unit" \ o -> do
      ty <- o .: "type"
      case ty :: Text of
        "pre-existing" -> PU_Preexisting <$> preExisting o
        "configured"   -> PU_Configured  <$> configured  o
        _              -> error $
          "parseJSON PlanUnit: unexpected type " ++ Text.unpack ty ++ ",\n\
          \expecting 'pre-existing' or 'configured'"
      where
        preExisting o = do
          puId      <- o .: "id"
          puPkgName <- o .: "pkg-name"
          puVersion <- o .: "pkg-version"
          puDepends <- o .: "depends"
          return $ PreexistingUnit {..}

        configured o = do
           puId      <- o .:  "id"
           puPkgName <- o .:  "pkg-name"
           puVersion <- o .:  "pkg-version"
           puFlags   <- fromMaybe (FlagSpec Map.empty) <$> o .:? "flags"
           mbComps   <- o .:? "components"
           (puComponentName, puDepends, puExeDepends, puSetupDepends) <-
             case mbComps of
               Nothing -> do
                 deps     <- o .: "depends"
                 exeDeps  <- o .: "exe-depends"
                 compName <- o .: "component-name"
                 let
                   comp
                     | compName == "lib"
                     = ComponentName Lib (unPkgName puPkgName)
                     | (ty,nm) <- Text.break (== ':') compName
                     , Just compTy <- parseComponentType ty
                     , not $ Text.null nm
                     = ComponentName compTy (Text.drop 1 nm)
                     | otherwise
                     = error $ "parseJSON PlanUnit: unsupported component name "
                            <> Text.unpack compName
                 return (comp, deps, exeDeps, [])
               Just comps -> do
                 lib       <- comps .:  "lib"
                 deps      <- lib   .:  "depends"
                 exeDeps   <- lib   .:  "exe-depends"
                 mbSetup   <- comps .:? "setup"
                 setupDeps <-
                   case mbSetup of
                     Nothing    -> return []
                     Just setup -> setup .: "depends"
                 return (ComponentName Lib (unPkgName puPkgName), deps, exeDeps, setupDeps)
           return $ ConfiguredUnit {..}

--------------------------

-- | A collection of cabal constraints, e.g. @>= 3.2 && < 3.4@.
newtype Constraints = Constraints Text
  deriving stock Show

instance Semigroup Constraints where
  Constraints c1 <> Constraints c2 =
    Constraints ( " ( " <> c1 <> " ) && ( " <> c2 <> " )" )

-- | A mapping from a package name to its flags and constraints.
type PkgSpecs = Strict.Map PkgName PkgSpec

-- | A mapping from a package name to its flags, constraints,
-- and components we want to build from it.
type UnitSpecs = Strict.Map PkgName (PkgSpec, Set ComponentName)

-- | A collection of allow-newer specifications, e.g. @pkg1:pkg2,*:base@.
newtype AllowNewer = AllowNewer ( Set (Text, Text) )
  deriving stock Show
  deriving newtype ( Semigroup, Monoid )

-- | Constraints and flags for a package.
data PkgSpec = PkgSpec { psConstraints :: Maybe Constraints
                       , psFlags :: FlagSpec
                       }
  deriving stock Show

parsePkgSpec :: Text -> PkgSpec
parsePkgSpec l = parseSpec Map.empty ( Text.words l )
  where
    parseSpec :: Strict.Map Text Bool -> [Text] -> PkgSpec
    parseSpec flags []
      = PkgSpec { psConstraints = Nothing
                , psFlags       = FlagSpec flags }
    parseSpec flags (w:ws)
      | Just (s,f) <- Text.uncons w
      , s == '+' || s == '-'
      = parseSpec (Map.insert f (s == '+') flags) ws
      | otherwise
      = PkgSpec { psConstraints = Just $ Constraints (Text.unwords (w:ws))
                , psFlags       = FlagSpec flags }

instance Semigroup PkgSpec where
  ( PkgSpec c1 f1 ) <> ( PkgSpec c2 f2 ) =
    PkgSpec ( c1 <> c2 )
            ( f1 <> f2 )

-- | Left-biased union of two sets of packages,
-- overriding flags and constraints of the second argument
-- with those provided in the first argument.
unionPkgSpecsOverriding :: PkgSpecs -> PkgSpecs -> PkgSpecs
unionPkgSpecsOverriding = Map.unionWith unionPkgSpec

-- | Combine two 'UnitSpecs'. Combines constraints and flags.
unionUnitSpecsCombining :: UnitSpecs -> UnitSpecs -> UnitSpecs
unionUnitSpecsCombining = Map.unionWith (<>)

-- | Left-biased union of package flags and constraints.
unionPkgSpec :: PkgSpec -> PkgSpec -> PkgSpec
unionPkgSpec (PkgSpec strongCts strongFlags) (PkgSpec weakCts weakFlags)
  = PkgSpec cts (strongFlags <> weakFlags)
    where
      cts = case strongCts of
        Nothing -> weakCts
        _       -> strongCts

-- | Binary data underlying a @cabal@ @plan.json@ file.
newtype CabalPlanBinary = CabalPlanBinary Lazy.ByteString

-- | Decode a 'CabalPlanBinary' into a 'CabalPlan'.
parsePlanBinary :: CabalPlanBinary -> CabalPlan
parsePlanBinary (CabalPlanBinary pb) =
  case eitherDecode pb of
    Left  err  -> error ("parsePlanBinary: failed to parse plan JSON\n" ++ err)
    Right plan -> plan
