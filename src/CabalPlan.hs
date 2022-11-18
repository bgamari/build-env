{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CabalPlan
    ( CabalPlan(..)
    , PkgId(..)
    , PkgName(..)
    , FlagSpec(..)
    , ComponentName(..)
    , showFlagSpec
    , PlanUnit(..)
    ) where

import Data.Aeson
import Data.Version
import qualified Data.Text as T
import qualified Data.Map.Strict as M

data CabalPlan = CabalPlan { planUnits :: [PlanUnit] }
    deriving (Show)

instance FromJSON CabalPlan where
    parseJSON = withObject "cabal plan" $ \o ->
        CabalPlan <$> o .: "install-plan"

newtype PkgId = PkgId T.Text
    deriving (Eq, Ord, Show, FromJSON)

newtype PkgName = PkgName { unPkgName :: T.Text }
    deriving (Eq, Ord, Show, FromJSON)

newtype FlagSpec = FlagSpec (M.Map T.Text Bool)
    deriving (Eq, Ord, Show, Semigroup, Monoid, FromJSON)

showFlagSpec :: FlagSpec -> String
showFlagSpec (FlagSpec fs) =
    unwords
    [ sign <> T.unpack flag
    | (flag, value) <- M.toList fs
    , let sign = if value then "+" else "-"
    ]

newtype ComponentName = ComponentName { unComponentName :: T.Text }
    deriving (Eq, Ord, Show)

data PlanUnit
    = PreexistingUnit { puId :: PkgId
                      , puPkgName :: PkgName
                      , puVersion :: Version
                      , puDepends :: [PkgId]
                      } 
    | ConfiguredUnit { puId :: PkgId
                     , puPkgName :: PkgName
                     , puVersion :: Version
                     , puComponentName :: ComponentName
                     , puFlags :: FlagSpec
                     , puDepends :: [PkgId]
                     , puSetupDepends :: [PkgId]
                     }
    deriving (Show)

instance FromJSON PlanUnit where
    parseJSON = withObject "plan unit" $ \o -> do
        ty <- o .: "type"
        case ty :: T.Text of
          "pre-existing" -> preExisting o
          "configured" -> configured o
      where
        preExisting o = do
            puId <- o .: "id"
            puPkgName <- o .: "pkg-name"
            puVersion <- o .: "pkg-version"
            puDepends <- o .: "depends"
            return $ PreexistingUnit {..}

        configured o = do
            puId <- o .: "id"
            puPkgName <- o .: "pkg-name"
            puVersion <- o .: "pkg-version"
            compName <- o .: "component-name"
            let puComponentName = ComponentName $ case compName of
                                    "lib" -> unPkgName puPkgName
                                    other -> other
            puFlags <- o .: "flags"
            puDepends <- o .: "depends"
            --puSetupDepends <- o .: "setup-depends"
            let puSetupDepends = []
            return $ ConfiguredUnit {..}
