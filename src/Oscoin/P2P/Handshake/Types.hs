module Oscoin.P2P.Handshake.Types
    ( Handshake
    , HandshakeRole (..)
    , HandshakeResult (..)
    , mapHandshakeResult
    )
where

import           Oscoin.Prelude

import           Oscoin.P2P.Connection (Socket')

data HandshakeRole = Acceptor | Connector

type Handshake e n i o =
       HandshakeRole
    -> Socket'
    -> Maybe n       -- ^ 'Just' if the peer id is known and must match 'hrPeerId'
    -> IO (Either e (HandshakeResult n i o))

data HandshakeResult n i o = HandshakeResult
    { hrPeerId :: n
    , hrSend   :: i -> IO o
    , hrRecv   :: o -> IO i
    }

mapHandshakeResult
    :: (n  -> n')
    -> (o  -> IO o')
    -> (o' -> IO o )
    -> HandshakeResult n  i o
    -> HandshakeResult n' i o'
mapHandshakeResult f h g = mapHandshakeResult' f (>=> h) (<=< g)

mapHandshakeResult'
    :: (n -> n')
    -> ((i -> IO o) -> (i' -> IO o'))
    -> ((o -> IO i) -> (o' -> IO i'))
    -> HandshakeResult n  i  o
    -> HandshakeResult n' i' o'
mapHandshakeResult' f h g r = r
    { hrPeerId = f (hrPeerId r)
    , hrSend   = h (hrSend   r)
    , hrRecv   = g (hrRecv   r)
    }
