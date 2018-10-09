module Oscoin.P2P
    (-- * Gossip
      withGossip
    , Gossip.runGossipT

    -- * Re-exports
    , module Oscoin.P2P.Class
    , module Oscoin.P2P.Types
    ) where

import           Oscoin.Prelude hiding (show)

import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash, blockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Logging (Logger)
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage

import           Oscoin.P2P.Class
import qualified Oscoin.P2P.Gossip as Gossip
import qualified Oscoin.P2P.Gossip.Broadcast as Bcast
import qualified Oscoin.P2P.Gossip.Membership as Membership
import           Oscoin.P2P.Handshake (Handshake)
import           Oscoin.P2P.Types

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import qualified Control.Concurrent.Async as Async
import           Data.ByteString.Lazy (fromStrict, toStrict)

-- | Start listening to gossip and pass the gossip handle to the runner.
--
-- When the runner returns we stop listening and return the runner result.
withGossip
    :: ( Serialise       tx
       , Crypto.Hashable tx
       , Exception e
       , Serialise o
       )
    => Logger
    -> NodeAddr
    -- ^ Node identity (\"self\")
    -> [NodeAddr]
    -- ^ Initial peers to connect to
    -> Storage tx IO
    -> Handshake e NodeId (Gossip.WireMessage (Gossip.ProtocolMessage Gossip.Peer)) o
    -> (Gossip.Handle e Gossip.Peer o -> IO a)
    -> IO a
withGossip logger selfAddr peerAddrs storage handshake run = do
    (self:peers) <-
        for (selfAddr:peerAddrs) $ \NodeAddr{..} ->
            Gossip.knownPeer nodeId nodeHost nodePort
    Gossip.withGossip
        logger
        self
        scheduleInterval
        (storageAsCallbacks storage)
        handshake
        Membership.defaultConfig
        (listenAndRun peers)
  where
    listenAndRun peers gossipHandle =
        Async.withAsync (listen peers gossipHandle) . const $ run gossipHandle

    listen peers = flip Gossip.runGossipT $
        Gossip.listen (nodeHost selfAddr) (nodePort selfAddr) peers

    scheduleInterval = 10

--------------------------------------------------------------------------------

storageAsCallbacks
    :: ( Serialise       tx
       , Crypto.Hashable tx
       )
    => Storage tx IO
    -> Bcast.Callbacks
storageAsCallbacks Storage{..} = Bcast.Callbacks
    { applyMessage  = wrapApply  storageApplyBlock  storageApplyTx
    , lookupMessage = wrapLookup storageLookupBlock storageLookupTx
    }

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
