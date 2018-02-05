module Oscoin.Org where

import           Oscoin.Prelude
import           Oscoin.State.Tree (Tree, Key, Val, Path)
import qualified Oscoin.State.Tree as Tree

import           Data.Aeson ( FromJSON(..), ToJSON(..)
                            , (.:), (.=), withObject, object
                            )

type OrgId = Text
type OrgKey = Key
type OrgPath = [Text]
type OrgVal = Val
type OrgTree = Tree OrgPath OrgVal

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

mkOrgPath :: OrgId -> Path -> OrgPath
mkOrgPath org path =
    "orgs" : org : path

mkOrgDataPath :: OrgId -> OrgPath -> OrgPath
mkOrgDataPath org =
    mkOrgPath org . mkDataPath

mkDataPath :: Path -> Path
mkDataPath = (:) "data"

getPath :: OrgId -> OrgPath -> OrgTree -> Maybe OrgVal
getPath org = Tree.get . mkOrgPath org

getDataPath :: OrgId -> OrgPath -> OrgTree -> Maybe OrgVal
getDataPath org = getPath org . mkDataPath

setPath :: OrgId -> OrgPath -> OrgVal -> OrgTree -> OrgTree
setPath org = Tree.set . mkOrgPath org
