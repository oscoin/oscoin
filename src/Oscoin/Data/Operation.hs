module Oscoin.Data.Operation where

import           Oscoin.Prelude

import           Codec.Serialise
import           Oscoin.Crypto.PubKey

data Operation msg = Operation
    { opMessage :: Signed msg
    , opSigner  :: PublicKey
    } deriving (Show, Generic)

instance Serialise msg => Serialise (Operation msg)
