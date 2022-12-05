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

data PlanUnit
  = PU_Preexisting PreexistingUnit
  | PU_Configured  ConfiguredUnit
  deriving stock Show

configuredUnitMaybe :: PlanUnit -> Maybe ConfiguredUnit
configuredUnitMaybe (PU_Configured pu)  = Just pu
configuredUnitMaybe (PU_Preexisting {}) = Nothing

planUnitId :: PlanUnit -> UnitId
planUnitId (PU_Preexisting (PreexistingUnit { puId })) = puId
planUnitId (PU_Configured  (ConfiguredUnit  { puId })) = puId

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

-- | A mapping from a package name to its flags and constraints.
type PkgSpecs = Strict.Map PkgName PkgSpec

-- | A list of allow-newer specifications, e.g. @pkg1:pkg2,*:base@.
newtype AllowNewer = AllowNewer [(Text, Text)]
  deriving stock Show
  deriving newtype ( Semigroup, Monoid )

-- | Constraints and flags for a package.
data PkgSpec = PkgSpec { psConstraints :: Maybe Constraints
                       , psFlags :: FlagSpec
                       }
  deriving stock Show

-- | Left-biased union of two sets of packages,
-- overriding flags and constraints of the second argument
-- with those provided in the first argument.
unionPkgSpecs :: PkgSpecs -> PkgSpecs -> PkgSpecs
unionPkgSpecs = Map.unionWith unionPkgSpec

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
