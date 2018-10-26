module Oscoin.P2P.Class (MonadBroadcast(..)) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.P2P.Types (Msg)

import           Codec.Serialise (Serialise)

class Monad m => MonadBroadcast m where
    broadcast :: (Serialise s, Serialise tx, Hashable tx) => Msg tx s -> m ()
