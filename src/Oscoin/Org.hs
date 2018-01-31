module Oscoin.Org where

import           Oscoin.Prelude
import           Oscoin.Storage.State (Key, Val)

import           Data.Aeson ( FromJSON(..), ToJSON(..)
                            , (.:), (.=), withObject, object
                            )

type OrgId = Text
type OrgKey = Text
type OrgVal = Val

type MemberId = Text

data Org = Org
    { orgId   :: OrgId
    , orgName :: Text
    } deriving (Show, Eq)

instance FromJSON Org where
    parseJSON =
        withObject "org" $ \o -> do
            orgId   <- o .: "id"
            orgName <- o .: "name"
            pure Org{..}

instance ToJSON Org where
    toJSON Org{..} =
        object [ "id"   .= orgId
               , "name" .= orgName
               ]

mkOrgKey :: OrgId -> OrgKey -> Key
mkOrgKey org key =
    ["orgs", org, key]
