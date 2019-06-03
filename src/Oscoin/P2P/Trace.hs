module Oscoin.P2P.Trace
    ( Traceable (..)
    , P2PEvent (..)
    )
where

import           Oscoin.P2P.Disco.Trace (DiscoEvent)
import           Oscoin.P2P.Handshake.Trace (HandshakeEvent)
import qualified Oscoin.P2P.Types as Types (ConversionError)

import qualified Network.Gossip.IO.Trace as Gossip

data Traceable n
    = TraceDisco     DiscoEvent
    | TraceGossip    (Gossip.Traceable n)
    | TraceHandshake (HandshakeEvent n)
    | TraceP2P       P2PEvent

data P2PEvent
    = ConversionError Types.ConversionError
    | NodeIsolated
