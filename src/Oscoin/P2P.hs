{-# LANGUAGE TupleSections #-}

module Oscoin.P2P
    ( Msg (..)

    -- * Gossip
    , runGossip
    , broadcast
    , storageCallbacks

    -- * Re-exports
    , module Oscoin.P2P.Types
    ) where

import           Oscoin.Prelude hiding (show)

import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash, blockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Logging (Logger)
import qualified Oscoin.Storage as Storage

import qualified Oscoin.P2P.Gossip as Gossip
import qualified Oscoin.P2P.Gossip.Broadcast as Bcast
import qualified Oscoin.P2P.Gossip.Handshake as Handshake
import qualified Oscoin.P2P.Gossip.Membership as Membership
import           Oscoin.P2P.Types

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import qualified Control.Concurrent.Async as Async
import           Data.ByteString.Lazy (fromStrict, toStrict)

data Msg tx =
      BlockMsg (Block tx ())
    | TxMsg    tx
    deriving (Eq, Generic)

instance Serialise tx => Serialise (Msg tx)

data MsgId tx =
      BlockId BlockHash
    | TxId    (Crypto.Hashed tx)
    deriving (Eq, Generic)

instance Serialise tx => Serialise (MsgId tx)

type GossipHandle = Gossip.Handle Handshake.HandshakeError Gossip.Peer

-- | Start listening to gossip and pass the gossip handle to the
-- runner. If the runner returns we stop listening and return the
-- runner result.
runGossip
    :: Logger
    -> Crypto.KeyPair
    -> NodeAddr
    -> [NodeAddr]
    -- ^ Initial peers to connect to
    -> Bcast.Callbacks
    -> (GossipHandle -> IO a)
    -> IO a
runGossip logger keypair selfAddr peerAddrs broadcastCallbacks run = do
    (self:peers) <-
        for (selfAddr:peerAddrs) $ \NodeAddr{..} ->
            Gossip.knownPeer nodeId nodeHost nodePort
    Gossip.withGossip
        logger
        keypair
        self
        scheduleInterval
        broadcastCallbacks
        Handshake.simple
        Membership.defaultConfig
        (listenAndRun peers)
  where
    listenAndRun peers gossipHandle =
        Async.withAsync (listen peers gossipHandle) $ \_ ->
            run gossipHandle

    listen peers =
        runReaderT $ Gossip.listen (nodeHost selfAddr) (nodePort selfAddr) peers

    scheduleInterval = 10

broadcast
    :: (Serialise tx, Crypto.Hashable tx)
    => Gossip.Handle e Gossip.Peer
    -> Msg tx
    -> IO ()
broadcast hdl msg = uncurry (Gossip.broadcast hdl) $ toGossip msg

storageCallbacks
    :: ( Serialise       tx
       , Crypto.Hashable tx
       )
    => (Block tx ()      -> IO Storage.ApplyResult)
    -> (tx               -> IO Storage.ApplyResult)
    -> (BlockHash        -> IO (Maybe (Block tx ())))
    -> (Crypto.Hashed tx -> IO (Maybe tx))
    -> Bcast.Callbacks
storageCallbacks applyBlock applyTx lookupBlock lookupTx = Bcast.Callbacks
    { applyMessage  = wrapApply  applyBlock  applyTx
    , lookupMessage = wrapLookup lookupBlock lookupTx
    }

--------------------------------------------------------------------------------

wrapApply
    :: ( Serialise       tx
       , Crypto.Hashable tx
       )
    => (Block tx () -> IO Storage.ApplyResult)
    -> (tx          -> IO Storage.ApplyResult)
    -> Bcast.MessageId
    -> ByteString
    -> IO Bcast.ApplyResult
wrapApply applyBlock applyTx mid payload =
    case fromGossip mid payload of
        Left  _   -> pure Bcast.Error -- TODO(kim): log error here (can't do anything about it)
        Right msg -> map convertApplyResult $
            case msg of
                BlockMsg blk -> applyBlock blk
                TxMsg    tx  -> applyTx tx
  where
    convertApplyResult = \case
        Storage.Applied -> Bcast.Applied
        Storage.Stale   -> Bcast.Stale
        Storage.Error   -> Bcast.Error

wrapLookup
    :: Serialise tx
    => (BlockHash -> IO (Maybe (Block tx ())))
    -> (Crypto.Hashed tx -> IO (Maybe tx))
    -> Bcast.MessageId
    -> IO (Maybe ByteString)
wrapLookup lookupBlock lookupTx mid =
    case deserialiseMessageId mid of
        Right (BlockId i) -> map (toStrict . CBOR.serialise) <$> lookupBlock i
        Right (TxId txId) -> map (toStrict . CBOR.serialise) <$> lookupTx txId
        Left _ -> pure Nothing

data ConversionError =
      DeserialiseFailure CBOR.DeserialiseFailure
    | IdPayloadMismatch

fromGossip
    :: (Serialise tx, Crypto.Hashable tx)
    => Bcast.MessageId
    -> ByteString
    -> Either ConversionError (Msg tx)
fromGossip mid payload = do
    mid' <- first DeserialiseFailure $ deserialiseMessageId mid
    msg  <- first DeserialiseFailure $ deserialisePayload payload

    case (mid', msg) of
        (BlockId hsh, BlockMsg blk) | hsh == blockHash  blk -> pure msg
        (TxId    hsh, TxMsg     tx) | hsh == Crypto.hash tx -> pure msg
        _ -> Left IdPayloadMismatch

toGossip
    :: ( Serialise       tx
       , Crypto.Hashable tx
       )
    => Msg tx
    -> (Bcast.MessageId, ByteString)
toGossip msg =
    bimap toStrict toStrict . (,CBOR.serialise msg) $
        case msg of
            BlockMsg blk -> CBOR.serialise $ blockHash blk
            TxMsg    tx  -> CBOR.serialise $ Crypto.hash tx

deserialiseMessageId
    :: Serialise tx
    => Bcast.MessageId
    -> Either CBOR.DeserialiseFailure (MsgId tx)
deserialiseMessageId = CBOR.deserialiseOrFail . fromStrict

deserialisePayload
    :: Serialise tx
    => ByteString
    -> Either CBOR.DeserialiseFailure (Msg tx)
deserialisePayload = CBOR.deserialiseOrFail . fromStrict
