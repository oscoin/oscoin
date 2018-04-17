module Oscoin.Account
    ( Account(..)
    , AccId
    , MemberId
    , getPath
    , setPath
    , getDataPath
    , pattern AccountsPrefix
    ) where

import           Oscoin.Prelude
import           Oscoin.State.Tree (Tree, Path)
import qualified Oscoin.State.Tree as Tree

import           Data.Aeson ( FromJSON(..), ToJSON(..)
                            , (.:), (.=), withObject, object
                            )

type AccId = Text
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

pattern AccountsPrefix :: (IsString a, Eq a) => a
pattern AccountsPrefix = "accounts"
-- pattern DataPrefix     = "data" :: Text

-- TODO: Rewrite with `Id Account`.
getPath :: AccId -> Path -> Tree Path v -> Maybe v
getPath acc path = Tree.get (AccountsPrefix : acc : path)

getDataPath :: AccId -> Path -> Tree Path v -> Maybe v
getDataPath acc path = getPath acc ("data" : path)

setPath :: AccId -> Path -> v -> Tree Path v -> Tree Path v
setPath acc path = Tree.set ("accounts" : acc : path)
