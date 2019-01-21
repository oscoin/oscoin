module Oscoin.Telemetry
    ( -- * Types
      TelemetryStore -- opaque
    , HasTelemetry(..)

    -- * API
    , newTelemetryStore
    , emit

    -- * Handy re-exports
    , module Oscoin.Telemetry.Events
    ) where

import           Oscoin.Prelude

import           Data.Text.Lazy.Builder (Builder)
import           Formatting
import           Formatting.Buildable as F
import           GHC.Stack as GHC
import           Lens.Micro

import           Oscoin.Crypto.Hash (formatHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.P2P.Types (fmtLogConversionError)
import           Oscoin.Telemetry.Events
import           Oscoin.Telemetry.Internal (TelemetryStore(..))
import           Oscoin.Telemetry.Logging as Log
import           Oscoin.Telemetry.Metrics

{------------------------------------------------------------------------------
  Typeclasses
------------------------------------------------------------------------------}

class HasTelemetry a where
    telemetryStoreL :: SimpleGetter a TelemetryStore

{------------------------------------------------------------------------------
  Single, unified API for both logging and metrics recording.
------------------------------------------------------------------------------}

-- | Creates a new 'TelemetryStore' from a 'Logger' and a 'MetricStore'.
newTelemetryStore :: Logger -> MetricsStore -> TelemetryStore
newTelemetryStore = TelemetryStore

-- | Single /facade/ to the telemetry API. Given a 'NotableEvent',
-- dispatch the updates to the metrics and logging systems.
---
--- >>> emit telemetryStore (BlockMinedEvent (blockHash b))
--
emit :: HasCallStack => TelemetryStore -> NotableEvent -> IO ()
emit TelemetryStore{..} evt = withLogger $ GHC.withFrozenCallStack $ do
    forM_ (toActions evt) (lift . updateMetricsStore telemetryMetrics)
    case evt of
        BlockReceivedEvent blockHash ->
            Log.debugM "received block"
                       (ftag "block_hash" % formatHash) blockHash
        BlockMinedEvent blockHash ->
            Log.withNamespace "node" $
                Log.infoM "mined block" formatBlockHash blockHash
        BlockAppliedEvent blockHash ->
            Log.debugM "applied block" formatBlockHash blockHash
        BlockStaleEvent   blockHash ->
            Log.infoM "stale block" formatBlockHash blockHash
        BlockApplyErrorEvent blockHash ->
            Log.errM "block application failed" formatBlockHash blockHash
        BlockOrphanEvent  blockHash ->
            Log.infoM "orphan block" formatBlockHash blockHash
        BlockValidationFailedEvent  blockHash _validationError ->
            Log.errM "block failed validation" formatBlockHash blockHash
        BlockEvaluationFailedEvent  blockHash _evalError ->
            Log.errM "block failed evaluation" formatBlockHash blockHash
        TxSentEvent txHash ->
            Log.withNamespace "p2p" $
                Log.infoM "tx sent"
                          (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxSubmittedEvent txHash ->
            Log.withNamespace "http-api" $
                Log.infoM "tx submitted"
                          (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxReceivedEvent txHash ->
            Log.debugM "tx received "
                       (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxStaleEvent txHash ->
            Log.infoM "tx wasn't applied as it was stale"
                      (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxAppliedEvent txHash ->
            Log.debugM "tx was correctly applied"
                       (ftag "tx_hash" % formatHash) (Crypto.fromHashed txHash)
        TxsAddedToMempoolEvent txs ->
            let hashes = map (Crypto.fromHashed . Crypto.hash) txs
            in Log.debugM "txs added to the mempool"
                          (ftag "tx_hashes" % listOf formatHash) hashes
        TxsRemovedFromMempoolEvent txs ->
            let hashes = map (Crypto.fromHashed . Crypto.hash) txs
            in Log.debugM "txs removed from the mempool"
                          (ftag "tx_hashes" % listOf formatHash) hashes
        Peer2PeerErrorEvent conversionError ->
            Log.errM "P2P error" fmtLogConversionError conversionError
        HttpApiRequest _req _status ->
            Log.withNamespace "http-api" $
                Log.infoM "todo" (now mempty)
  where
    withLogger :: ReaderT Log.Logger IO a -> IO a
    withLogger = flip runReaderT telemetryLogger

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
    BlockValidationFailedEvent _ _validationError -> [
        -- NOTE(adn) Do we want an Histogram to categorise the different
        -- validation errors or at the very least some labels to understand
        -- which one is which?
        CounterIncrease "oscoin.blocks_failed_validation.total" noLabels
     ]
    BlockEvaluationFailedEvent _ _evalError -> [
        -- NOTE(adn) Do we want an Histogram to categorise the different
        -- validation errors or at the very least some labels to understand
        -- which one is which?
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
    Peer2PeerErrorEvent _ -> [
        -- FIXME(adn) Here is an example where we would like to have the
        -- same metric but with different labels (one for each erorr), so that
        -- we could aggregate this properly with something like Prometheus.
        CounterIncrease "oscoin.p2p.errors.total" noLabels
     ]
    HttpApiRequest _req _status -> [
        CounterIncrease "oscoin.api.http_requests.total" noLabels
     ]


{------------------------------------------------------------------------------
  Utility functions
-------------------------------------------------------------------------------}

-- | Given a 'Format' which knows how to render to a 'Builder' the elements
-- of a list, return a 'Format' which knows how to format the whole list.
listOf :: Format Builder (a -> Builder) -> Format r ([a] -> r)
listOf formatElement = later (F.build . map (bprint formatElement))

formatBlockHash :: Format r (Crypto.Hash -> r)
formatBlockHash = ftag "block_hash" % formatHash
