module Oscoin.P2P.Class (MonadBroadcast(..)) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.P2P.Types (Msg)

import           Codec.Serialise (Serialise)

class Monad m => MonadBroadcast c m where
    broadcast :: (Serialise s, Serialise tx, Hashable c tx) => Msg c tx s -> m ()
