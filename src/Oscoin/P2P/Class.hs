module Oscoin.P2P.Class
    ( MonadBroadcast(..)
    )
where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (blockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.P2P.Gossip (GossipT)
import qualified Oscoin.P2P.Gossip as Gossip
import           Oscoin.P2P.Types (Msg(..))

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import           Data.ByteString.Lazy (toStrict)

class Monad m => MonadBroadcast m where
    broadcast :: (Serialise tx, Crypto.Hashable tx) => Msg tx -> m ()

instance MonadIO m => MonadBroadcast (GossipT e m) where
    broadcast msg =
        uncurry Gossip.broadcast
            . bimap toStrict toStrict . (,CBOR.serialise msg)
            $ case msg of
                BlockMsg blk -> CBOR.serialise $ blockHash blk
                TxMsg    tx  -> CBOR.serialise $ Crypto.hash tx
    {-# INLINE broadcast #-}
