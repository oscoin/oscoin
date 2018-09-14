module Oscoin.P2P.Gossip.Wire
    ( WireMessage (..)
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)

data WireMessage p =
      WirePayload p
    | WireGoaway (Maybe Text)
    deriving (Generic)

instance Serialise p => Serialise (WireMessage p)
