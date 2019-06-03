module Oscoin.P2P.Handshake.Trace
    ( HandshakeEvent (..)
    )
where

import           Oscoin.Prelude

import qualified Network.Gossip.IO.Peer as Gossip (Peer)
import           Network.Socket (SockAddr)

data HandshakeEvent n =
      HandshakeError    SockAddr SomeException
    | HandshakeComplete (Gossip.Peer n)

