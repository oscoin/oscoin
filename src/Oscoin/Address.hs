module Oscoin.Address
    ( Address
    , toAddress
    ) where

import Oscoin.Prelude
import Oscoin.Crypto.PubKey

import Data.ByteString.Base58
import Data.ByteString.Lazy (toStrict)
import Data.Binary (encode)

newtype Address = Address ByteString
    deriving (Show, Eq, Ord)

toAddress :: PublicKey -> Address
toAddress pk = Address . encodeBase58 bitcoinAlphabet . toStrict $ encode pk
