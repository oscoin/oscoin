module Oscoin.HTTP.API.Result where

import           Oscoin.Prelude

import qualified Data.Aeson as Aeson
import qualified Codec.Serialise as Serial

data Result a =
      Ok  a
    | Err Text
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON a => Aeson.ToJSON (Result a)
instance Aeson.FromJSON a => Aeson.FromJSON (Result a)
instance Serial.Serialise a => Serial.Serialise (Result a)

ok :: a -> Result a
ok = Ok

err :: Text -> Result b
err = Err

isOk :: Result a -> Bool
isOk (Ok _) = True
isOk _      = False

isErr :: Result a -> Bool
isErr = not . isOk
