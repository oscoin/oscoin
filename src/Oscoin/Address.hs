module Oscoin.Address
    ( Address
    , toAddress
    ) where

import Oscoin.Prelude
import Oscoin.Crypto.PubKey

import Data.ByteString.Base58
import Data.ByteString.Lazy (toStrict)
import Data.Binary (Binary, encode)
import Data.Aeson (ToJSON(..))

newtype Address = Address ByteString
    deriving (Show, Eq, Ord, Binary)

instance ToJSON Address where
    toJSON (Address bs) = toJSON (decodeUtf8 bs)

toAddress :: PublicKey -> Address
toAddress pk = Address . encodeBase58 bitcoinAlphabet . toStrict $ encode pk
