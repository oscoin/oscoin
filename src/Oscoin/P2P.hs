module Oscoin.P2P
    (-- * Gossip
      GossipT
    , withGossip
    , runGossipT

    -- * Re-exports
    , module Oscoin.P2P.Class
    , module Oscoin.P2P.Types
    ) where

import           Oscoin.Prelude hiding (show)

import           Oscoin.Clock (MonadClock)
import           Oscoin.Crypto.Blockchain.Block
                 (Block, BlockHash, blockHash, blockHeader, blockPrevHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import           Oscoin.Telemetry (NotableEvent(..), TelemetryStore, emit)

import           Oscoin.P2P.Class
import           Oscoin.P2P.Handshake
import qualified Oscoin.P2P.Transport as Transport
import           Oscoin.P2P.Types

import qualified Network.Gossip.HyParView as Membership
import qualified Network.Gossip.HyParView.Periodic as Periodic
import qualified Network.Gossip.IO.Peer as Gossip
import qualified Network.Gossip.IO.Run as Gossip.Run
import qualified Network.Gossip.IO.Socket as Gossip.Socket
import qualified Network.Gossip.IO.Wire as Gossip
import qualified Network.Gossip.Plumtree as Bcast

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Network.Socket (SockAddr, Socket)

type Wire = Gossip.WireMessage (Gossip.Run.ProtocolMessage (Gossip.Peer NodeId))

newtype GossipT m a = GossipT (ReaderT (Gossip.Run.Env NodeId) m a)
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadIO
             , MonadTrans
             , MonadReader (Gossip.Run.Env NodeId)
             )

instance MonadIO m => MonadBroadcast (GossipT m) where
    broadcast msg = do
        env <- ask
        liftIO $
            uncurry (Gossip.Run.broadcast env)
                . bimap toStrict toStrict . (,CBOR.serialise msg)
                $ case msg of
                    BlockMsg blk -> CBOR.serialise $ blockHash blk
                    TxMsg    tx  -> CBOR.serialise $ Crypto.hash tx
    {-# INLINE broadcast #-}

instance MonadClock m => MonadClock (GossipT m)

runGossipT :: Gossip.Run.Env NodeId -> GossipT m a -> m a
runGossipT r (GossipT ma) = runReaderT ma r

-- | Start listening to gossip and pass the gossip handle to the runner.
--
-- When the runner returns we stop listening and return the runner result.
withGossip
    :: ( Serialise s
       , Serialise       tx
       , Crypto.Hashable tx
       , Exception e
       , Serialise o
       )
    => TelemetryStore
    -> NodeAddr
    -- ^ Node identity (\"self\")
    -> [NodeAddr]
    -- ^ Initial peers to connect to
    -> Storage tx s IO
    -> Handshake e NodeId Wire o
    -> (Gossip.Run.Env NodeId -> IO a)
    -> IO a
withGossip telemetryStore selfAddr peerAddrs Storage{..} handshake run = do
    (self:peers) <-
        for (selfAddr:peerAddrs) $ \NodeAddr{..} ->
            Gossip.knownPeer nodeId nodeHost nodePort
    Gossip.Run.withGossip
        self
        Membership.defaultConfig
        Periodic.defaultConfig
        scheduleInterval
        (wrapHandshake handshake)
        (wrapApply telemetryStore storageLookupBlock storageApplyBlock storageApplyTx)
        (wrapLookup storageLookupBlock storageLookupTx)
        (nodeHost selfAddr)
        (nodePort selfAddr)
        peers
        run
  where
    scheduleInterval = 10

--------------------------------------------------------------------------------

wrapHandshake
    :: ( Exception e
       , Serialise o
       )
    => Handshake e NodeId Wire o
    -> Gossip.Socket.HandshakeRole
    -> Socket
    -> SockAddr
    -> Maybe NodeId
    -> IO (Gossip.Socket.Connection NodeId Wire)
wrapHandshake handshake role sock addr psk = do
    hres <-
        runHandshakeT (Transport.framed sock) $
            handshake (mapHandshakeRole role) psk
    case hres of
        Left  e -> throwM e
        Right r -> do
            mutex <- newMVar ()
            let transp = Transport.streamingEnvelope (hrPreSend r) (hrPostRecv r)
                       $ Transport.streaming sock
            pure Gossip.Socket.Connection
                { Gossip.Socket.connPeer = Gossip.Peer
                    { Gossip.peerNodeId = hrPeerId r
                    , Gossip.peerAddr   = addr
                    }
                , Gossip.Socket.connSend  = withMVar mutex . const . Transport.streamingSend transp
                , Gossip.Socket.connRecv  = Transport.streamingRecv transp
                , Gossip.Socket.connClose = pure ()
                }
  where
    mapHandshakeRole Gossip.Socket.Acceptor  = Acceptor
    mapHandshakeRole Gossip.Socket.Connector = Connector

wrapApply
    :: ( Serialise       s
       , Serialise       tx
       , Crypto.Hashable tx
       )
    => TelemetryStore
    -> (BlockHash   -> IO (Maybe (Block tx s)))
    -> (Block tx s  -> IO Storage.ApplyResult)
    -> (tx          -> IO Storage.ApplyResult)
    -> Bcast.MessageId
    -> ByteString
    -> IO Bcast.ApplyResult
wrapApply telemetryStore lookupBlock applyBlock applyTx mid payload =
    case fromGossip mid payload of
        Left  _   -> pure Bcast.Error -- TODO(kim): log error here (can't do anything about it)
        Right msg -> map (uncurry convertApplyResult) $
            case msg of
                TxMsg    tx  -> do
                    result <- applyTx tx
                    forM_ (TxReceivedEvent (Crypto.hash tx) : telemetryEvents result)
                          (emit telemetryStore)
                    (,) Nothing <$> pure result
                BlockMsg blk ->
                    let
                        parentHash = blockPrevHash $ blockHeader blk
                        parentId   = toStrict $ CBOR.serialise parentHash
                        -- If we didn't find it, indicate 'parent' is missing.
                        -- Otherwise, 'Nothing' is missing.
                        missing    = maybe (Just parentId) (const Nothing)
                     in
                        liftA2 (,)
                               (missing <$> lookupBlock parentHash)
                               (do result <- applyBlock blk
                                   forM_ (BlockReceivedEvent (blockHash blk) : telemetryEvents result)
                                         (emit telemetryStore)
                                   pure result
                               )
  where
    telemetryEvents = \case
        Storage.Applied evts -> evts
        Storage.Stale evts   -> evts
        Storage.Error evts   -> evts
    convertApplyResult missing = \case
        Storage.Applied _ -> Bcast.Applied missing
        Storage.Stale   _ -> Bcast.Stale   missing
        Storage.Error   _ -> Bcast.Error

wrapLookup
    :: (Serialise tx, Serialise s)
    => (BlockHash -> IO (Maybe (Block tx s)))
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
    :: (Serialise s, Serialise tx, Crypto.Hashable tx)
    => Bcast.MessageId
    -> ByteString
    -> Either ConversionError (Msg tx s)
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
    :: (Serialise tx, Serialise s)
    => ByteString
    -> Either CBOR.DeserialiseFailure (Msg tx s)
deserialisePayload = CBOR.deserialiseOrFail . fromStrict
