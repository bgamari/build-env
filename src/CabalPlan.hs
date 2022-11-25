{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CabalPlan where

-- base
import Data.Maybe
  ( fromMaybe, mapMaybe )
import Data.Version
  ( Version )

-- aeson
import Data.Aeson
--  ( FromJSON(..), (.:), withObject )

-- bytestring
import qualified Data.ByteString.Lazy as Lazy
  ( ByteString )

-- containers
import qualified Data.Map.Strict as Strict
  ( Map )
import qualified Data.Map.Strict as Map
  ( toList )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( unpack )

-------------------------------------------------------------------------------

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
    deriving newtype (Eq, Ord, FromJSON)

newtype PkgName = PkgName { unPkgName :: Text }
    deriving stock   Show
    deriving newtype (Eq, Ord, FromJSON)

newtype FlagSpec = FlagSpec (Strict.Map Text Bool)
    deriving stock   Show
    deriving newtype (Eq, Ord, Semigroup, Monoid, FromJSON)

showFlagSpec :: FlagSpec -> String
showFlagSpec (FlagSpec fs) =
    unwords
    [ sign <> Text.unpack flag
    | (flag, value) <- Map.toList fs
    , let sign = if value then "+" else "-"
    ]

flagSpecIsEmpty :: FlagSpec -> Bool
flagSpecIsEmpty (FlagSpec fs) = null fs

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
            puId      <- o .: "id"
            puPkgName <- o .: "pkg-name"
            puVersion <- o .: "pkg-version"
            compName  <- fromMaybe "lib" <$> o .:? "component-name"
            let puComponentName =
                  ComponentName $
                    case compName of
                      "lib" -> unPkgName puPkgName
                      other -> other
            puFlags        <- o .: "flags"
            puDepends      <- o .: "depends"
            puSetupDepends <- fromMaybe [] <$> o .:? "setup-depends"
            return $ ConfiguredUnit {..}

--------------------------

newtype Constraints = Constraints Text
  deriving stock Show

type PkgSpecs = Strict.Map PkgName PkgSpec

newtype AllowNewer = AllowNewer [(Text, Text)]
  deriving stock Show

data PkgSpec = PkgSpec { psConstraints :: Maybe Constraints
                       , psFlags :: FlagSpec
                       }
  deriving stock Show

newtype CabalPlanBinary = CabalPlanBinary Lazy.ByteString
