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
  ( all, pack, unpack, unwords )

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

newtype PkgId = PkgId { unPkgId :: Text }
    deriving stock Show
    deriving newtype (Eq, Ord, FromJSON, FromJSONKey)

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

-- | The name of a cabal component, e.g. @lib:comp@.
newtype ComponentName = ComponentName { unComponentName :: Text }
    deriving stock (Eq, Ord, Show)

data PlanUnit
  = PU_Preexisting PreexistingUnit
  | PU_Configured  ConfiguredUnit
  deriving stock Show

configuredUnitMaybe :: PlanUnit -> Maybe ConfiguredUnit
configuredUnitMaybe (PU_Configured pu)  = Just pu
configuredUnitMaybe (PU_Preexisting {}) = Nothing

planUnitId :: PlanUnit -> PkgId
planUnitId (PU_Preexisting (PreexistingUnit { puId })) = puId
planUnitId (PU_Configured  (ConfiguredUnit  { puId })) = puId

-- | All the dependencies of a unit: @depends@ and @setup-depends@.
allDepends :: ConfiguredUnit -> [PkgId]
allDepends (ConfiguredUnit{puDepends, puSetupDepends}) =
  puDepends ++ puSetupDepends

data PreexistingUnit
  = PreexistingUnit
    { puId      :: PkgId
    , puPkgName :: PkgName
    , puVersion :: Version
    , puDepends :: [PkgId]
    }
  deriving stock Show

data ConfiguredUnit
  = ConfiguredUnit
    { puId            :: PkgId
    , puPkgName       :: PkgName
    , puVersion       :: Version
    , puComponentName :: ComponentName
    , puFlags         :: FlagSpec
    , puDepends       :: [PkgId]
    , puSetupDepends  :: [PkgId]
    }
  deriving stock Show

instance FromJSON PlanUnit where
    parseJSON = withObject "plan unit" \ o -> do
        ty <- o .: "type"
        case ty :: Text of
          "pre-existing" -> PU_Preexisting <$> preExisting o
          "configured"   -> PU_Configured  <$> configured  o
          _              -> error $ "parseJSON PlanUnit: unexpected type " ++ Text.unpack ty ++ ",\n\
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
            mbComps   <- o .:? "components"
            (compName, puDepends, puSetupDepends) <-
              case mbComps of
                Nothing -> do
                  deps     <- o .: "depends"
                  compName <- o .: "component-name"
                  return (compName, deps, [])
                Just comps -> do
                  lib       <- comps .:  "lib"
                  deps      <- lib   .:  "depends"
                  mbSetup   <- comps .:? "setup"
                  setupDeps <-
                    case mbSetup of
                      Nothing    -> return []
                      Just setup -> setup .: "depends"
                  return ("lib", deps, setupDeps)
            let puComponentName =
                  ComponentName $
                    case compName of
                      "lib" -> unPkgName puPkgName
                      other -> other
            puFlags        <- fromMaybe (FlagSpec Map.empty) <$> o .:? "flags"
            return $ ConfiguredUnit {..}

--------------------------

-- | A collection of cabal constraints, e.g. @>= 3.2 && < 3.4@.
--
--
newtype Constraints = Constraints Text
  deriving stock Show

-- | A mapping from package to its flags.
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

-- | Binary data underlying a @cabal@ @plan.json@ file.
newtype CabalPlanBinary = CabalPlanBinary Lazy.ByteString

-- | Decode a 'CabalPlanBinary' into a 'CabalPlan'.
parsePlanBinary :: CabalPlanBinary -> CabalPlan
parsePlanBinary (CabalPlanBinary pb) =
  case eitherDecode pb of
    Left  err  -> error ("parsePlanBinary: failed to parse plan JSON\n" ++ err)
    Right plan -> plan
