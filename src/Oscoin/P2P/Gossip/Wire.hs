module Oscoin.P2P.Gossip.Wire
    ( WireMessage (..)
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)

data WireMessage b m =
      WireBroadcast  b
    | WireMembership m
    | WireGoaway (Maybe Text)
    deriving (Generic)

instance (Serialise b, Serialise m) => Serialise (WireMessage b m)
