module Oscoin.Telemetry
    ( -- * Types
      Handle -- opaque
    , HasTelemetry(..)

    -- * API
    , newTelemetryStore
    , emit

    -- * Handy re-exports
    , module Oscoin.Telemetry.Events

    ) where

import           Oscoin.Prelude

import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import           Formatting
import           Formatting.Buildable as F
import           GHC.Stack as GHC
import           Lens.Micro
import           Network.HTTP.Types as HTTP
import           Network.Socket (SockAddr)
import           Network.Wai as HTTP

import qualified Network.Gossip.HyParView as Gossip.HPV
import qualified Network.Gossip.IO.Peer as Gossip (Peer, peerAddr, peerNodeId)
import qualified Network.Gossip.IO.Protocol as Gossip
import qualified Network.Gossip.IO.Trace as Gossip
import qualified Network.Gossip.Plumtree as Gossip.EBT

import qualified Oscoin.Consensus.Types as Consensus
import           Oscoin.Crypto.Blockchain.Block (prettyDifficulty)
import qualified Oscoin.Crypto.Blockchain.Eval as Eval
import           Oscoin.Crypto.Hash (formatHash, formatHashed)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey (publicKeyHash)
import           Oscoin.P2P.Types (fmtLogConversionError)
import qualified Oscoin.P2P.Types as P2P
import           Oscoin.Telemetry.Events
import           Oscoin.Telemetry.Internal (Handle(..))
import           Oscoin.Telemetry.Logging as Log
import           Oscoin.Telemetry.Metrics
import           Oscoin.Time as Time

{------------------------------------------------------------------------------
  Typeclasses
------------------------------------------------------------------------------}

class HasTelemetry a where
    telemetryStoreL :: SimpleGetter a Handle

{------------------------------------------------------------------------------
  Single, unified API for both logging and metrics recording.
------------------------------------------------------------------------------}

-- | Creates a new 'Handle' from a 'Logger' and a 'MetricStore'.
newTelemetryStore :: Logger -> MetricsStore -> Handle
newTelemetryStore = Handle

-- | Single /facade/ to the telemetry API. Given a 'NotableEvent',
-- dispatch the updates to the metrics and logging systems.
---
--- >>> emit telemetryStore (BlockMinedEvent (blockHash b))
--
emit :: HasCallStack => Handle -> NotableEvent -> IO ()
emit Handle{..} evt = withLogger $ GHC.withFrozenCallStack $ do
    forM_ (toActions evt) (lift . updateMetricsStore telemetryMetrics)
    case evt of
        BlockReceivedEvent blockHash ->
            Log.withNamespace "p2p" $
                Log.debugM "received block" fmtBlockHash blockHash
        BlockMinedEvent blockHash ->
            Log.withNamespace "node" $
                Log.infoM "mined block" fmtBlockHash blockHash
        BlockAppliedEvent blockHash ->
            Log.withNamespace "storage" $
                Log.debugM "applied block" fmtBlockHash blockHash
        BlockStaleEvent blockHash ->
            Log.withNamespace "storage" $
                Log.infoM "stale block" fmtBlockHash blockHash
        BlockApplyErrorEvent blockHash ->
            Log.errM "block application failed" fmtBlockHash blockHash
        BlockOrphanEvent  blockHash ->
            Log.withNamespace "storage" $
                Log.infoM "orphan block" fmtBlockHash blockHash
        BlockValidationFailedEvent  blockHash validationError ->
            Log.withNamespace "storage" $
                Log.errM "block failed validation"
                         (fmtBlockHash % " " % fmtValidationError)
                         blockHash
                         validationError
        BlockEvaluationFailedEvent blockHash evalError ->
            Log.errM "block failed evaluation"
                     (fmtBlockHash % " " % ferror Eval.fromEvalError)
                     blockHash
                     evalError
        TxSentEvent txHash ->
            Log.withNamespace "p2p" $
                Log.infoM "tx sent"
                          (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxSubmittedEvent txHash ->
            Log.withNamespace "http-api" $
                Log.infoM "tx submitted"
                          (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxReceivedEvent txHash ->
            Log.withNamespace "p2p" $
                Log.debugM "tx received "
                           (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxStaleEvent txHash ->
            Log.withNamespace "storage" $
                Log.infoM "tx wasn't applied as it was stale"
                          (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxAppliedEvent txHash ->
            Log.withNamespace "storage" $
                Log.debugM "tx was correctly applied"
                           (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxsAddedToMempoolEvent txs ->
            let hashes = map (Crypto.fromHashed . Crypto.hash) txs
            in Log.debugM "txs added to the mempool"
                          (ftag "tx_hashes" % listOf formatHash) hashes
        TxsRemovedFromMempoolEvent txs ->
            let hashes = map (Crypto.fromHashed . Crypto.hash) txs
            in Log.withNamespace "storage" $
                   Log.debugM "txs removed from the mempool"
                              (ftag "tx_hashes" % listOf formatHash) hashes
        Peer2PeerErrorEvent conversionError ->
            Log.withNamespace "p2p" $
                Log.errM "P2P error" fmtLogConversionError conversionError
        HttpApiRequest req status duration ->
            let fmt = ftag "status" % fmtStatus % " " %
                      ftag "method" % fmtMethod % " " %
                      ftag "path"   % fmtPath   % " " %
                      ftag "params" % fmtParams % " " %
                      ftag "service" % fmtDuration
            in Log.withNamespace "http-api" $
                Log.infoM ""
                          fmt
                          status
                          (HTTP.requestMethod req)
                          (HTTP.pathInfo req)
                          (HTTP.queryString req)
                          duration

        GossipEvent    e -> Log.withNamespace "gossip" $ traceGossip e
        HandshakeEvent e -> Log.withNamespace "p2p"    $ traceHandshake e
  where
    withLogger :: ReaderT Log.Logger IO a -> IO a
    withLogger = flip runReaderT telemetryLogger

    traceGossip = \case
        Gossip.TraceBootstrap e -> case e of
            Gossip.Bootstrapping self _others ->
                Log.infoM "bootstrapping" fmtPeer self
            Gossip.Bootstrapped self ->
                Log.infoM "bootstrapped" fmtPeer self

        Gossip.TraceConnection e -> case e of
            Gossip.Connecting peer ->
                Log.infoM "connecting" fmtPeer peer
            Gossip.Connected peer ->
                Log.infoM "connected" fmtPeer peer
            Gossip.ConnectFailed peer ex ->
                Log.errM "failed to connect"
                         (fmtPeer % " " % fexception)
                         peer
                         ex
            Gossip.ConnectionLost peer ex ->
                Log.infoM "connection reset by peer"
                          (fmtPeer % " " % fexception)
                          peer
                          ex
            Gossip.ConnectionAccepted addr ->
                Log.infoM "incoming connection" (ftag "addr" % fmtSockAddr) addr
            Gossip.Disconnected peer ->
                Log.infoM "disconnected" fmtPeer peer

        Gossip.TraceMembership e ->
            let
                (msg, peer) = case e of
                    Gossip.Promoted p ->
                        ("peer promoted to active view", p)
                    Gossip.Demoted p ->
                        ("peer demoted from active view", p)
             in
                Log.debugM msg fmtPeer peer

        Gossip.TraceWire (Gossip.ProtocolError rcpt ex) ->
            Log.errM "error sending" (fmtPeer % " " % fexception) rcpt ex
        Gossip.TraceWire e ->
            let
                payloadInfo = \case
                   Gossip.ProtocolPlumtree rpc ->
                       case Gossip.EBT.rpcPayload rpc of
                           Gossip.EBT.Gossip{} -> "gossip"
                           Gossip.EBT.IHave{}  -> "ihave"
                           Gossip.EBT.Prune    -> "prune"
                           Gossip.EBT.Graft{}  -> "graft"

                   Gossip.ProtocolHyParView rpc ->
                       case Gossip.HPV.rpcPayload rpc of
                           Gossip.HPV.Join           -> "join"
                           Gossip.HPV.ForwardJoin{}  -> "forward join"
                           Gossip.HPV.Disconnect     -> "disconnect"
                           Gossip.HPV.Neighbor{}     -> "neighbor"
                           Gossip.HPV.NeighborReject -> "neighbor reject"
                           Gossip.HPV.Shuffle{}      -> "shuffle"
                           Gossip.HPV.ShuffleReply{} -> "shuffle reply"

                (msg, pl, peer) = case e of
                    Gossip.ProtocolRecv from rpc ->
                        ("received", payloadInfo rpc, from)
                    Gossip.ProtocolSend to' rpc ->
                        ("sending", payloadInfo rpc, to')
                    _ -> panic "Gossip.ProtocolError not handled"
             in
                Log.debugM msg (stext % " " % fmtPeer) pl peer

    traceHandshake = \case
        P2P.HandshakeError addr ex ->
            Log.errM "error during handshake"
                     (ftag "addr" % fmtSockAddr % " " % fexception)
                     addr
                     ex
        P2P.HandshakeComplete peer ->
            Log.debugM "handshake complete" fmtPeer peer

-- | Maps each 'NotableEvent' to a set of 'Action's. The big pattern-matching
-- block is by design. Despite the repetition (once in 'emit' and once in
-- this function) it guides library authors to be reminded of which points they
-- need to modify in the code each time a new 'NotableEvent' is added.
--
-- NOTE(adn) Perhaps we want an 'Histogram' which tracks the total number
-- of blocks and \"buckets them\" according to the fact the successfully
-- applied or not?
toActions :: NotableEvent -> [Action]
toActions = \case
    BlockReceivedEvent _ -> [
        CounterIncrease "oscoin.blocks_received.total" noLabels
      ]
    BlockMinedEvent _ -> [
        CounterIncrease "oscoin.blocks_mined.total" noLabels
      ]
    BlockAppliedEvent _ -> [
        CounterIncrease "oscoin.blocks_applied.total" noLabels
     ]
    BlockStaleEvent _ -> [
        CounterIncrease "oscoin.blocks_stale.total" noLabels
     ]
    BlockOrphanEvent _ -> [
        CounterIncrease "oscoin.blocks_orphan.total" noLabels
     ]
    BlockApplyErrorEvent _ -> [
        CounterIncrease "oscoin.blocks_failed_to_apply.total" noLabels
     ]
    BlockValidationFailedEvent _ validationError -> [
        CounterIncrease "oscoin.blocks_failed_validation.total" $
            validationErrorToLabels validationError
     ]
    BlockEvaluationFailedEvent _ _evalError -> [
        CounterIncrease "oscoin.blocks_failed_evaluation.total" noLabels
     ]
    TxSentEvent _ -> [
        CounterIncrease "oscoin.storage.txs_sent.total" noLabels
     ]
    TxSubmittedEvent _ -> [
        CounterIncrease "oscoin.api.http_requests.total" noLabels
     ]
    TxReceivedEvent _ -> [
        CounterIncrease "oscoin.storage.txs_received.total" noLabels
     ]
    TxAppliedEvent _ -> [
        CounterIncrease "oscoin.storage.txs_applied.total" noLabels
     ]
    TxStaleEvent _ -> [
        CounterIncrease "oscoin.storage.txs_stale.total" noLabels
     ]
    TxsAddedToMempoolEvent txs -> [
        GaugeAdd "oscoin.mempool.txs.total" noLabels (fromIntegral $ length txs)
     ]
    TxsRemovedFromMempoolEvent txs -> [
        GaugeAdd "oscoin.mempool.txs.total" noLabels (- fromIntegral (length txs))
     ]
    Peer2PeerErrorEvent p2pErr -> [
        CounterIncrease "oscoin.p2p.errors.total" $ p2pErrorToLabels p2pErr

     ]
    HttpApiRequest req status duration -> [
        CounterIncrease "oscoin.api.http_requests.total" $
            labelsFromList [ ("method", sformat fmtMethod (HTTP.requestMethod req))
                           , ("status", sformat fmtStatus status)
                           ]
     , HistogramObserve "oscoin.api.http_requests.seconds"
                        noLabels
                        defaultBuckets
                        (fromIntegral duration / fromIntegral Time.seconds)
     ]

    GossipEvent e -> case e of
        Gossip.TraceConnection evt -> case evt of
            Gossip.Connected{} ->
                [ CounterIncrease
                    "oscoin.p2p.gossip.connection.connect_success.total"
                    noLabels
                ]
            Gossip.ConnectFailed{} ->
                [ CounterIncrease
                    "oscoin.p2p.gossip.connection.connect_errors.total"
                    noLabels
                ]
            Gossip.ConnectionAccepted{} ->
                [ CounterIncrease
                    "oscoin.p2p.gossip.connection.accepted.total"
                    noLabels
                ]
            Gossip.ConnectionLost{} ->
                [ CounterIncrease
                    "oscoin.p2p.gossip.connection.reset_by_peer.total"
                    noLabels
                ]
            Gossip.Disconnected{} ->
                [ CounterIncrease
                    "oscoin.p2p.gossip.connection.disconnected.total"
                    noLabels
                ]
            _ -> []

        Gossip.TraceWire evt -> case evt of
            Gossip.ProtocolError{} ->
                [ CounterIncrease
                    "oscoin.p2p.gossip.protocol.errors.total"
                    noLabels
                ]
            -- For protocol messages, we measure the total number exchanged, and
            -- separately actual payload (vs. control) messages.
            Gossip.ProtocolRecv _ msg ->
                  CounterIncrease
                    "oscoin.p2p.gossip.protocol.received.total"
                    noLabels
                : case msg of
                    Gossip.ProtocolPlumtree
                        Gossip.EBT.RPC { Gossip.EBT.rpcPayload = Gossip.EBT.Gossip{} } ->
                        [ CounterIncrease
                            "oscoin.p2p.gossip.protocol.received.payload.total"
                            noLabels
                        ]
                    _ -> []
            Gossip.ProtocolSend _ msg ->
                  CounterIncrease
                    "oscoin.p2p.gossip.protocol.sent.total"
                    noLabels
                : case msg of
                    Gossip.ProtocolPlumtree
                        Gossip.EBT.RPC { Gossip.EBT.rpcPayload = Gossip.EBT.Gossip{} } ->
                        [ CounterIncrease
                            "oscoin.p2p.gossip.protocol.sent.payload.total"
                            noLabels
                        ]
                    _ -> []

        _ -> []

    HandshakeEvent e -> case e of
        P2P.HandshakeError{} ->
            [ CounterIncrease
                "oscoin.p2p.handshake.errors.total"
                noLabels
            ]
        P2P.HandshakeComplete{} ->
            [ CounterIncrease
                "oscoin.p2p.handshake.completions.total"
                noLabels
            ]


{------------------------------------------------------------------------------
  Utility functions
-------------------------------------------------------------------------------}

-- | Given a 'Format' which knows how to render to a 'Builder' the elements
-- of a list, return a 'Format' which knows how to format the whole list.
listOf :: Format Builder (a -> Builder) -> Format r ([a] -> r)
listOf formatElement = later (F.build . map (bprint formatElement))

{------------------------------------------------------------------------------
  Formatters
-------------------------------------------------------------------------------}

fmtBlockHash :: Format r (Crypto.Hash -> r)
fmtBlockHash = ftag "block_hash" % formatHash

fmtValidationError :: Format r (Consensus.ValidationError -> r)
fmtValidationError = ferror $ \case
    Consensus.InvalidParentHash parentHash ->
        sformat ("Parent hash " % formatHash % " was invalid") parentHash
    Consensus.InvalidDataHash dataHash ->
        sformat ("Data hash " % formatHash % " was invalid.") dataHash
    Consensus.InvalidTargetDifficulty expected actual ->
        sformat ("Invalid target difficulty: expecting " % stext % " but got " % stext)
                (prettyDifficulty expected)
                (prettyDifficulty actual)
    Consensus.InvalidBlockDifficulty block target  ->
        sformat ("Block difficulty " % stext % " doesn't match target " % stext)
                (prettyDifficulty block)
                (prettyDifficulty target)
    Consensus.InvalidBlockTimestamp ts    ->
        sformat ("Block has invalid timestamp of " % stext) (prettyDuration ts)
    Consensus.InvalidBlockSize blockSize ->
        sformat ("Block has size" % int % " which exceeded the limit") blockSize

fmtStatus :: Format r (HTTP.Status -> r)
fmtStatus = mapf HTTP.statusCode int

fmtMethod :: Format r (HTTP.Method -> r)
fmtMethod = mapf toS string

fmtPath :: Format r ([Text] -> r)
fmtPath = mapf (mappend (T.singleton '/') . T.intercalate "/") stext

fmtParams :: Format r (HTTP.Query -> r)
fmtParams = mapf (T.pack . toS . HTTP.renderQuery False) fquoted

fmtDuration :: Format r (Duration -> r)
fmtDuration = mapf Time.prettyDuration stext

fmtNodeId :: Format r (P2P.NodeId -> r)
fmtNodeId = mapf (publicKeyHash . P2P.fromNodeId) formatHashed

fmtSockAddr :: Format r (SockAddr -> r)
fmtSockAddr = shown -- FIXME(kim)

fmtPeer :: Format r (Gossip.Peer P2P.NodeId -> r)
fmtPeer =
       mapf Gossip.peerAddr   (ftag "peer_addr"   % fmtSockAddr)
     % " "
    <> mapf Gossip.peerNodeId (ftag "peer_nodeid" % fmtNodeId)

{------------------------------------------------------------------------------
  Labels
-------------------------------------------------------------------------------}

validationErrorToLabels :: Consensus.ValidationError -> Labels
validationErrorToLabels validationError = labelsFromList $
    (:[]) . ("validation_error",) $ gderiveErrorClass validationError

p2pErrorToLabels :: P2P.ConversionError -> Labels
p2pErrorToLabels p2pError = labelsFromList $
    (:[]) . ("p2p_error",) $ gderiveErrorClass p2pError
