module Oscoin.Account where

import           Oscoin.Prelude
import           Oscoin.State.Tree (Tree, Key, Val, Path)
import qualified Oscoin.State.Tree as Tree

import           Data.Aeson ( FromJSON(..), ToJSON(..)
                            , (.:), (.=), withObject, object
                            )

type AccId = Text
type AccKey = Key
type AccPath = [Text]
type AccVal = Val
type AccTree = Tree AccPath AccVal

type MemberId = Text

data Account = Account
    { accId   :: AccId
    , accName :: Text
    } deriving (Show, Eq)

instance FromJSON Account where
    parseJSON =
        withObject "account" $ \o -> do
            accId   <- o .: "id"
            accName <- o .: "name"
            pure Account{..}

instance ToJSON Account where
    toJSON Account{..} =
        object [ "id"   .= accId
               , "name" .= accName
               ]

mkAccPath :: AccId -> Path -> AccPath
mkAccPath acc path =
    "accounts" : acc : path

mkAccDataPath :: AccId -> AccPath -> AccPath
mkAccDataPath acc =
    mkAccPath acc . mkDataPath

mkDataPath :: Path -> Path
mkDataPath = (:) "data"

getPath :: AccId -> AccPath -> AccTree -> Maybe AccVal
getPath acc = Tree.get . mkAccPath acc

getDataPath :: AccId -> AccPath -> AccTree -> Maybe AccVal
getDataPath acc = getPath acc . mkDataPath

setPath :: AccId -> AccPath -> AccVal -> AccTree -> AccTree
setPath acc = Tree.set . mkAccPath acc
