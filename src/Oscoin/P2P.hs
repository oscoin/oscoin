{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.P2P
    (-- * Gossip
      GossipT
    , withGossip
    , runGossipT
    , getPeers
    , getPeers'

    -- * Re-exports
    , module Oscoin.P2P.Class
    , module Oscoin.P2P.Types
    ) where

import           Oscoin.Prelude hiding (length, show)

import           Oscoin.Clock (MonadClock)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block
                 , BlockHash
                 , Sealed
                 , blockHash
                 , blockHeader
                 , blockPrevHash
                 )
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Storage (Storage(..))
import qualified Oscoin.Storage as Storage
import           Oscoin.Telemetry (NotableEvent(..), emit)
import qualified Oscoin.Telemetry as Telemetry

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
import           Data.ByteArray (ByteArrayAccess(..))
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import           Formatting.Buildable (Buildable)
import           Network.Socket (SockAddr, Socket)

type Wire c =
    Gossip.WireMessage (Gossip.Run.ProtocolMessage (Gossip.Peer (NodeId c)))

instance ( Hashable (Crypto.PublicKey c)
         , Eq (Crypto.PublicKey c)
         , Serialise (Crypto.PublicKey c)
         ) => ByteArrayAccess (Wire c) where
    length           = length . toStrict . CBOR.serialise
    withByteArray ba = withByteArray (toStrict $ CBOR.serialise ba)

newtype GossipT c m a = GossipT (ReaderT (Gossip.Run.Env (NodeId c)) m a)
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadIO
             , MonadTrans
             , MonadReader (Gossip.Run.Env (NodeId c))
             , MonadThrow
             , MonadCatch
             , MonadMask
             )

instance ( Hashable (Crypto.PublicKey c)
         , Eq (Crypto.PublicKey c)
         , Serialise (Crypto.Hash c)
         , MonadIO m
         ) => MonadBroadcast c (GossipT c m) where
    broadcast msg = do
        env <- ask
        liftIO $
            uncurry (Gossip.Run.broadcast env)
                . bimap (toStrict . CBOR.serialise) (toStrict . CBOR.serialise)
                $ case msg of
                    BlockMsg blk -> (BlockId (blockHash blk),  msg)
                    TxMsg    tx  -> (TxId (Crypto.hash @c tx), msg)
    {-# INLINE broadcast #-}

instance MonadClock m => MonadClock (GossipT c m)

runGossipT :: Gossip.Run.Env (NodeId c) -> GossipT c m a -> m a
runGossipT r (GossipT ma) = runReaderT ma r

-- | Start listening to gossip and pass the gossip handle to the runner.
--
-- When the runner returns we stop listening and return the runner result.
withGossip
    :: forall c tx e s o a.
       ( Crypto.Hashable c tx
       , Hashable (Crypto.PublicKey c)
       , Crypto.Hashable c (Crypto.PublicKey c)
       , Eq (Crypto.PublicKey c)
       , Exception e
       , Buildable (Crypto.Hash c)
       , Serialise s
       , Serialise tx
       , Serialise o
       , Serialise (BlockHash c)
       )
    => Telemetry.Handle
    -> SelfAddr c
    -> Set (Maybe (NodeId c), SockAddr)
    -> Storage c tx s IO
    -> Handshake e (NodeId c) (Wire c) o
    -> (Gossip.Run.Env (NodeId c) -> IO a)
    -> IO a
withGossip telemetryStore selfAddr peers Storage{..} handshake run = do
    self  <-
        Gossip.knownPeer
            (runIdentity (nodeId selfAddr))
            (hostToHostName (nodeHost selfAddr))
            (nodePort selfAddr)

    Gossip.Run.withGossip
        self
        Membership.defaultConfig
        Periodic.defaultConfig
        scheduleInterval
        (wrapHandshake @c telemetryStore handshake)
        (wrapApply telemetryStore storageLookupBlock storageApplyBlock storageApplyTx)
        (wrapLookup storageLookupBlock storageLookupTx)
        (Telemetry.emit telemetryStore . Telemetry.GossipEvent)
        (toList peers) -- TODO(kim): make this a 'Set' in gossip, too
        run
  where
    scheduleInterval = 10

--------------------------------------------------------------------------------

wrapHandshake
    :: forall c e o.
       ( Crypto.Hashable c (Crypto.PublicKey c)
       , Exception e
       , Serialise o
       , Buildable (Crypto.Hash c)
       )
    => Telemetry.Handle
    -> Handshake e (NodeId c) (Wire c) o
    -> Gossip.Socket.HandshakeRole
    -> Socket
    -> SockAddr
    -> Maybe (NodeId c)
    -> IO (Gossip.Socket.Connection (NodeId c) (Wire c))
wrapHandshake telemetry handshake role sock addr psk = do
    hres <-
        runHandshakeT (Transport.framed sock) $
            handshake (mapHandshakeRole role) psk
    case hres of
        Left  e -> do
            emit telemetry . Telemetry.HandshakeEvent @c $
                HandshakeError addr (toException e)
            throwM e
        Right r -> do
            let peer = Gossip.Peer
                        { Gossip.peerNodeId = hrPeerId r
                        , Gossip.peerAddr   = addr
                        }
            emit telemetry . Telemetry.HandshakeEvent $ HandshakeComplete peer
            mutex <- newMVar ()
            let transp = Transport.streamingEnvelope (hrPreSend r) (hrPostRecv r)
                       $ Transport.streaming sock
            pure Gossip.Socket.Connection
                { Gossip.Socket.connPeer = peer
                , Gossip.Socket.connSend  = withMVar mutex . const . Transport.streamingSend transp
                , Gossip.Socket.connRecv  = Transport.streamingRecv transp
                , Gossip.Socket.connClose = pure ()
                }
  where
    mapHandshakeRole Gossip.Socket.Acceptor  = Acceptor
    mapHandshakeRole Gossip.Socket.Connector = Connector

wrapApply
    :: forall c tx s.
       ( Crypto.Hashable c tx
       , Buildable (Crypto.Hash c)
       , Serialise s
       , Serialise tx
       , Serialise (BlockHash c)
       )
    => Telemetry.Handle
    -> (BlockHash c             -> IO (Maybe (Block c tx (Sealed c s))))
    -> (Block c tx (Sealed c s) -> IO Storage.ApplyResult)
    -> (tx                      -> IO Storage.ApplyResult)
    -> Bcast.MessageId
    -> ByteString
    -> IO Bcast.ApplyResult
wrapApply telemetryStore lookupBlock applyBlock applyTx mid payload =
    case fromGossip mid payload of
        Left conversionError -> do
            emit telemetryStore (Peer2PeerErrorEvent conversionError)
            pure Bcast.Error
        Right msg -> map (uncurry convertApplyResult) $
            case msg of
                TxMsg    tx  -> do
                    result <- applyTx tx
                    forM_ (TxReceivedEvent (Crypto.hash @c tx) : telemetryEvents result)
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
    :: ( Crypto.HasHashing c
       , Serialise s
       , Serialise tx
       , Serialise (BlockHash c)
       )
    => (BlockHash c -> IO (Maybe (Block c tx s)))
    -> (Crypto.Hashed c tx -> IO (Maybe tx))
    -> Bcast.MessageId
    -> IO (Maybe ByteString)
wrapLookup lookupBlock lookupTx mid =
    case deserialiseMessageId mid of
        Right (BlockId i) -> map (toStrict . CBOR.serialise) <$> lookupBlock i
        Right (TxId txId) -> map (toStrict . CBOR.serialise) <$> lookupTx txId
        Left _ -> pure Nothing

fromGossip
    :: ( Crypto.Hashable c tx
       , Serialise s
       , Serialise tx
       , Serialise (BlockHash c)
       )
    => Bcast.MessageId
    -> ByteString
    -> Either ConversionError (Msg c tx s)
fromGossip mid payload = do
    mid' <- first DeserialiseFailure $ deserialiseMessageId mid
    msg  <- first DeserialiseFailure $ deserialisePayload payload

    case (mid', msg) of
        (BlockId hsh, BlockMsg blk) | hsh == blockHash  blk -> pure msg
        (TxId    hsh, TxMsg     tx) | hsh == Crypto.hash tx -> pure msg
        _ -> Left IdPayloadMismatch

deserialiseMessageId
    :: ( Serialise tx
       , Serialise (BlockHash c)
       )
    => Bcast.MessageId
    -> Either CBOR.DeserialiseFailure (MsgId c tx)
deserialiseMessageId = CBOR.deserialiseOrFail . fromStrict

deserialisePayload
    :: ( Crypto.HasHashing c
       , Serialise tx
       , Serialise s
       , Serialise (BlockHash c)
       )
    => ByteString
    -> Either CBOR.DeserialiseFailure (Msg c tx s)
deserialisePayload = CBOR.deserialiseOrFail . fromStrict

-------------------------------------------------------------------------------
-- Membership queries
-------------------------------------------------------------------------------

getPeers
    :: ( Eq (Crypto.PublicKey c)
       , Hashable (Crypto.PublicKey c)
       , MonadIO m
       )
    => Gossip.Run.Env (NodeId c)
    -> m (HashSet (Gossip.Peer (NodeId c)))
getPeers = liftIO . Gossip.Run.getPeers

getPeers'
    :: ( Eq (Crypto.PublicKey c)
       , Hashable (Crypto.PublicKey c)
       , MonadIO m
       )
    => Gossip.Run.Env (NodeId c)
    -> m (Membership.Peers (Gossip.Peer (NodeId c)))
getPeers' = liftIO . Gossip.Run.getPeers'
