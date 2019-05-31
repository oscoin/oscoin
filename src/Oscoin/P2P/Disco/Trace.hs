module Oscoin.P2P.Disco.Trace
    ( DiscoEvent (..)
    )
where

import           Oscoin.Prelude

import qualified Oscoin.P2P.Disco.MDns as MDns

import           Data.IP (IP)
import qualified Network.DNS as DNS
import           Network.Socket (PortNumber)


data DiscoEvent =
      MDnsResponderEvent MDns.ResponderTrace
    | MDnsResolverEvent  MDns.ResolverTrace
    | AddrInfoError      IP PortNumber IOException
    | DNSError           DNS.DNSError
    | DiscoStartEvent
    | DiscoCompleteEvent
    deriving Show
