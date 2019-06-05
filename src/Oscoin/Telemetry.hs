module Oscoin.Telemetry
    ( -- * Types
      Handle -- opaque
    , HasTelemetry(..)

    -- * Tracing events
    , module Oscoin.Telemetry.Trace
    , telemetryProbe

    -- * API
    , newTelemetryStore
    , emit

    -- * Formatters
    , fmtBlockHash
    , fmtValidationError

    -- * Handy re-exports
    , module Oscoin.Telemetry.Events

    ) where

import           Oscoin.Prelude

import           Control.Concurrent.Async (AsyncCancelled(..))
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder)
import           Formatting
import           Formatting.Buildable as F
import           GHC.Stack as GHC
import           Lens.Micro (SimpleGetter)
import qualified Network.DNS as DNS
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
import           Oscoin.Crypto.PubKey (PublicKey)
import qualified Oscoin.P2P.Disco as Disco
import qualified Oscoin.P2P.Disco.MDns as Disco
import qualified Oscoin.P2P.Handshake.Trace as P2P (HandshakeEvent(..))
import qualified Oscoin.P2P.Trace as P2P (P2PEvent(..), Traceable(..))
import           Oscoin.P2P.Types (fmtLogConversionError)
import qualified Oscoin.P2P.Types as P2P
import           Oscoin.Protocol.Trace (NodeSyncEvent(..), ProtocolEvent(..))
import           Oscoin.Telemetry.Events
import           Oscoin.Telemetry.Internal (Handle(..))
import           Oscoin.Telemetry.Logging as Log
import           Oscoin.Telemetry.Metrics
import           Oscoin.Telemetry.Trace
import           Oscoin.Time as Time

{------------------------------------------------------------------------------
  Typeclasses
------------------------------------------------------------------------------}

class HasTelemetry a where
    telemetryStoreL :: SimpleGetter a Handle


{------------------------------------------------------------------------------
  Probe using the telemetry system.
------------------------------------------------------------------------------}
telemetryProbe :: Handle -> Probe IO
telemetryProbe h = Probe (emit h)

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
        BlockBroadcastFailedEvent blockHash e ->
            Log.withNamespace "node" $
                Log.errM "block broadcast failed"
                         (fmtBlockHash % " " % fexception)
                         blockHash
                         e
        BlockBroadcastEvent blockHash ->
            Log.withNamespace "node" $
                Log.infoM "broadcast block" fmtBlockHash blockHash
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
        DifficultyAdjustedEvent newDifficulty previousDifficulty ->
            let fmt = ftag "new" % stext % " "
                    % ftag "old" % stext % " "
                    % ftag "direction" % stext
            in Log.withNamespace "consensus" $
                   Log.infoM "difficulty adjustment"
                             fmt
                             (prettyDifficulty newDifficulty)
                             (prettyDifficulty previousDifficulty)
                             (if newDifficulty > previousDifficulty then "increasing" else "decreasing")
        TxSentEvent txHash ->
            Log.withNamespace "p2p" $
                Log.infoM "tx sent"
                          (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxSubmittedEvent txHash ->
            Log.withNamespace "http-api" $
                Log.infoM "tx submitted"
                          (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxSubmittedInvalidEvent txHash ->
            Log.withNamespace "http-api" $
                Log.infoM "invalid tx submitted"
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
        TxApplyInvalidEvent txHash ->
            Log.withNamespace "storage" $
                Log.debugM "invalid transaction"
                           (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxsAddedToMempoolEvent hashes ->
            Log.debugM "txs added to the mempool"
                       (ftag "tx_hashes" % listOf formatHash) hashes
        TxsRemovedFromMempoolEvent hashes ->
            Log.withNamespace "storage" $
                Log.debugM "txs removed from the mempool"
                           (ftag "tx_hashes" % listOf formatHash) hashes
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

        P2PEvent e -> handleP2P e
        ProtocolEvent ev -> handleProtocol ev
        NodeSyncEvent ev -> handleSync ev

  where
    withLogger :: ReaderT Log.Logger IO a -> IO a
    withLogger = flip runReaderT telemetryLogger

    handleP2P
        :: (Crypto.Hashable c (PublicKey c), Buildable (Crypto.Hash c))
        => P2P.Traceable (P2P.NodeInfo c)
        -> ReaderT Logger IO ()
    handleP2P = \case
        P2P.TraceDisco     ev -> handleDisco ev
        P2P.TraceGossip    ev -> handleGossip ev
        P2P.TraceHandshake ev -> handleHandshake ev
        P2P.TraceP2P       ev -> Log.withNamespace "p2p" $ case ev of
            P2P.ConversionError e ->
                Log.errM "Conversion failed" fmtLogConversionError e

            P2P.NodeIsolated ->
                Log.errM "Ran out of peers. Node isolated!" noFields

    handleDisco :: Disco.DiscoEvent -> ReaderT Logger IO ()
    handleDisco = Log.withNamespace "disco" . \case
        Disco.MDnsResponderEvent r -> case r of
            Disco.ResponderError e
              -- this happens on ^C, so not very interesting
              | Just AsyncCancelled <- fromException e -> pure ()
            Disco.ResponderError e ->
              Log.errM "mDNS responder error" fexception e
            Disco.ResponderWhatsTheQuestion _ sender ->
                Log.infoM "mDNS: invalid query type"
                          (ftag "sender" % fmtSockAddr)
                          sender
            Disco.ResponderHaveNoAnswer q sender ->
                Log.infoM "mDNS: received query for unknown SRV record"
                          ( ftag "q" % stext
                          % " "
                          % ftag "sender" % fmtSockAddr
                          )
                          (decodeUtf8With lenientDecode $ DNS.qname q)
                          sender
            -- TODO(kim): debug actual payload?
            Disco.ResponderRecv _ sender ->
                Log.infoM "mDNS: received query"
                          (ftag "sender" % fmtSockAddr)
                          sender
            Disco.ResponderSend _ recipient ->
                Log.debugM "mDNS: sending query"
                           (ftag "recipient" % fmtSockAddr)
                           recipient

        Disco.MDnsResolverEvent r -> case r of
            Disco.ResolverError e ->
                Log.errM "mDNS resolver error" fexception e
            Disco.ResolverRecv _ from ->
                Log.infoM "mDNS: received response"
                          (ftag "from" % fmtSockAddr)
                          from

        Disco.AddrInfoError ip port e ->
            Log.errM "getaddrinfo failed"
                     ( ftag "ip"   % shown
                     % ftag "port" % shown
                     % fexception
                     )
                     ip
                     port
                     e

        Disco.DNSError e ->
            Log.errM "DNS error" fexception e

        Disco.DiscoStartEvent ->
            Log.debugM "performing discovery..." noFields

        Disco.DiscoCompleteEvent ->
            Log.debugM "discovery complete" noFields

    handleGossip
        :: (Crypto.Hashable c (PublicKey c), Buildable (Crypto.Hash c))
        => Gossip.Traceable (P2P.NodeInfo c)
        -> ReaderT Logger IO ()
    handleGossip = Log.withNamespace "gossip" . \case
        Gossip.TraceBootstrap e -> case e of
            Gossip.Bootstrapping self _others ->
                Log.infoM "bootstrapping" fmtPeer self
            Gossip.Bootstrapped self ->
                Log.infoM "bootstrapped" fmtPeer self

        Gossip.TraceConnection e -> case e of
            Gossip.Connecting addr minfo ->
                Log.infoM "connecting"
                    ( ftag "addr" % fmtSockAddr
                    % " "
                    % ftag "nodeid" % later (maybe mempty (bprint fmtNodeId))
                    )
                    addr
                    (map P2P.nodeNodeId minfo)
            Gossip.Connected peer ->
                Log.infoM "connected" fmtPeer peer
            Gossip.ConnectFailed addr minfo ex ->
                Log.errM "failed to connect"
                         ( ftag "addr" % fmtSockAddr
                         % " "
                         % ftag "nodeid" % later (maybe mempty (bprint fmtNodeId))
                         % " "
                         % fexception
                         )
                         addr
                         (map P2P.nodeNodeId minfo)
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

    handleHandshake
        :: (Crypto.Hashable c (PublicKey c), Buildable (Crypto.Hash c))
        => P2P.HandshakeEvent (P2P.NodeInfo c)
        -> ReaderT Logger IO ()
    handleHandshake = Log.withNamespace "handshake" . \case
        P2P.HandshakeError addr ex ->
            Log.errM "error during handshake"
                     (ftag "addr" % fmtSockAddr % " " % fexception)
                     addr
                     ex
        P2P.HandshakeComplete peer ->
            Log.debugM "handshake complete" fmtPeer peer

    handleProtocol
        :: Buildable (Crypto.Hash c)
        => ProtocolEvent c
        -> ReaderT Logger IO ()
    handleProtocol = Log.withNamespace "protocol" . \case
        BlockExtendedTip newTip ->
            Log.infoM "Block extended tip"
                      ( ftag "new_tip"  % formatHash)
                      newTip
        BlockStoredAsOrphan orphanHash orphanageSize ->
            Log.debugM "Block stored as orphan"
                       (fmtBlockHash % " " %
                        ftag "orphanage_size" % int
                       )
                       orphanHash
                       orphanageSize
        RollbackOccurred depth newTip ->
            Log.debugM "Rollback occurred"
                       (ftag "depth" % int % " " %
                        ftag "new_tip" % formatHash
                       )
                       depth
                       newTip
        PotentialNewChainFound chainScore ->
            Log.debugM "Chain evaluated for adoption"
                       (ftag "candidate_score" % int)
                       chainScore
    handleSync
       :: Buildable (Crypto.Hash c)
       => NodeSyncEvent c
       -> ReaderT Logger IO ()
    handleSync = Log.withNamespace "sync" . \case
        NodeSyncStarted (localTip, localHeight) (remoteTip, remoteHeight) ->
            Log.infoM "Node syncing started"
                      ( ftag "local_tip"  % formatHash % " "
                      % ftag "local_height" % shown % " "
                      % ftag "remote_tip" % formatHash % " "
                      % ftag "remote_height" % shown % " "
                      )
                      localTip
                      localHeight
                      remoteTip
                      remoteHeight
        NodeSyncFinished (localTip, localHeight) ->
            Log.infoM "Node syncing finished"
                      ( ftag "local_tip"  % formatHash % " "
                      % ftag "local_height" % shown
                      )
                      localTip
                      localHeight
        NodeSyncMissing missing ->
            Log.infoM "Missing blocks to fetch elsewhere"
                      ( ftag "number"  % int )
                      missing
        NodeSyncFetched requested ->
            Log.infoM "Fetched blocks from peer"
                      ( ftag "number"  % int )
                      requested
        NodeSyncError ex ->
            Log.errM "Node syncing error" fexception ex

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
    BlockBroadcastFailedEvent _ _ -> [
        CounterIncrease "oscoin.blocks_sent.errors.total" noLabels
      ]
    BlockBroadcastEvent _ -> [
        CounterIncrease "oscoin.blocks_sent.total" noLabels
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
    -- NOTE(adn) If we find it useful, we could include also separate counters
    -- for how many times the difficulty increased or decreased.
    DifficultyAdjustedEvent _ _ -> [
        CounterIncrease "oscoin.consensus.difficulty_adjustments.total" noLabels
     ]
    TxSentEvent _ -> [
        CounterIncrease "oscoin.storage.txs_sent.total" noLabels
     ]
    TxSubmittedEvent _ -> [
        CounterIncrease "oscoin.api.http_requests.total" noLabels
     ]
    TxSubmittedInvalidEvent _ -> [
        CounterIncrease "oscoin.api.http_requests.total" noLabels
     ]
    TxReceivedEvent _ -> [
        CounterIncrease "oscoin.storage.txs_received.total" noLabels
     ]
    TxAppliedEvent _ -> [
        CounterIncrease "oscoin.storage.txs_applied.total" noLabels
     ]
    TxApplyInvalidEvent _ -> [
        CounterIncrease "oscoin.storage.txs_apply_invalid.total" noLabels
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
    P2PEvent ev -> p2pActions ev
    ProtocolEvent e -> case e of
        BlockExtendedTip _ -> []
        PotentialNewChainFound _ -> []
        BlockStoredAsOrphan _ orphanageSize -> [
          GaugeSet "oscoin.protocol.orphanage.size" noLabels (fromIntegral orphanageSize)
         ]
        RollbackOccurred _rollbackDepth _ -> [
          CounterIncrease "oscoin.protocol.rollbacks.total" noLabels
          -- TODO(adn) Use an histogram to bucket the depth of the rollback,
          -- so we can study how much we are rolling-back, on average.
         ]
    NodeSyncEvent e -> case e of
        NodeSyncStarted{} ->
            [ CounterIncrease
                "oscoin.protocol.sync.sync_iterations"
                noLabels
            ]
        _ -> []
  where
    p2pActions = \case
        P2P.TraceDisco     ev -> discoActions ev
        P2P.TraceGossip    ev -> gossipActions ev
        P2P.TraceHandshake ev -> handshakeActions ev
        P2P.TraceP2P       ev -> case ev of
            P2P.ConversionError e ->
                [ CounterIncrease "oscoin.p2p.conversion_errors.total" $
                    conversionErrorToLabels e
                ]
            P2P.NodeIsolated ->
                [ CounterIncrease "oscoin.p2p.network_partitions.total" noLabels
                ]

    discoActions = \case
        Disco.MDnsResponderEvent Disco.ResponderError{} ->
            [ CounterIncrease
                "oscoin.disco.mdns_responder.errors.total"
                noLabels
            ]
        Disco.MDnsResponderEvent Disco.ResponderRecv{} ->
            [ CounterIncrease
                "oscoin.disco.mdns_responder.requests.total"
                noLabels
            ]
        Disco.DiscoStartEvent ->
            [ CounterIncrease
                "oscoin.disco.discos.attempts.total"
                noLabels
            ]
        Disco.DiscoCompleteEvent ->
            [ CounterIncrease
                "oscoin.disco.discos.successes.total"
                noLabels
            ]
        _ -> []

    gossipActions = \case
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

    handshakeActions = \case
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

noFields :: Format r r
noFields = Formatting.now mempty

fmtBlockHash :: F.Buildable (Crypto.Hash c) => Format r (Crypto.Hash c -> r)
fmtBlockHash = ftag "block_hash" % formatHash

fmtValidationError
    :: F.Buildable (Crypto.Hash c)
    => Format r (Consensus.ValidationError c -> r)
fmtValidationError = ferror $ \case
    Consensus.InvalidHeight expected actual ->
        sformat ("Invalid height" % ftag "expected" % int % ftag "actual" % int)
                expected
                actual
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

fmtNodeId
    :: forall c r.
       ( Buildable (Crypto.Hash c)
       , Crypto.Hashable c (PublicKey c)
       )
    => Format r (P2P.NodeId c -> r)
fmtNodeId = mapf (Crypto.hash @c . P2P.fromNodeId) formatHashed

fmtSockAddr :: Format r (SockAddr -> r)
fmtSockAddr = shown -- FIXME(kim)

fmtPeer
    :: forall c r.
       ( Crypto.Hashable c (PublicKey c)
       , Buildable (Crypto.Hash c)
       )
    => Format r (Gossip.Peer (P2P.NodeInfo c) -> r)
fmtPeer =
       mapf Gossip.peerAddr   (ftag "peer_addr"   % fmtSockAddr)
     % " "
    <> mapf (P2P.nodeNodeId . Gossip.peerNodeId) (ftag "peer_nodeid" % fmtNodeId @c)

{------------------------------------------------------------------------------
  Labels
-------------------------------------------------------------------------------}

validationErrorToLabels :: Consensus.ValidationError c -> Labels
validationErrorToLabels validationError = labelsFromList $
    (:[]) . ("validation_error",) $ gderiveErrorClass validationError

conversionErrorToLabels :: P2P.ConversionError -> Labels
conversionErrorToLabels p2pError = labelsFromList $
    (:[]) . ("p2p_error",) $ gderiveErrorClass p2pError
