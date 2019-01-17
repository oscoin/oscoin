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
emit TelemetryStore{..} evt = GHC.withFrozenCallStack $ do
    forM_ (toActions evt) (updateMetricsStore telemetryMetrics)
    case evt of
        BlockReceivedEvent blockHash ->
            Log.debug telemetryLogger ("received block " % formatHash) blockHash
        BlockMinedEvent blockHash ->
            Log.info telemetryLogger ("mined block " % formatHash) blockHash
        BlockAppliedEvent blockHash ->
            Log.debug telemetryLogger ("applied block " % formatHash) blockHash
        BlockStaleEvent   blockHash ->
            -- TODO(adn) Perhaps we want a 'warning' logging level here?
            -- Think about implementing it as part of oscoin#337.
            Log.info telemetryLogger ("stale block " % formatHash) blockHash
        BlockApplyErrorEvent blockHash ->
            Log.err telemetryLogger ("block application failed" % formatHash) blockHash
        BlockOrphanEvent  blockHash ->
            -- TODO(adn) 'warning' logging level here?
            Log.info telemetryLogger ("orphan block " % formatHash) blockHash
        BlockValidationFailedEvent  blockHash _validationError ->
            Log.err telemetryLogger
                    ("block failed validation " % formatHash) blockHash
        BlockEvaluationFailedEvent  blockHash _evalError ->
            Log.err telemetryLogger
                    ("block failed evaluation " % formatHash) blockHash
        TxSentEvent txHash ->
            Log.info telemetryLogger
                     ("tx sent " % formatHash)
                     (Crypto.fromHashed txHash)
        TxReceivedEvent txHash ->
            Log.debug telemetryLogger
                      ("tx received " % formatHash)
                      (Crypto.fromHashed txHash)
        TxStaleEvent txHash ->
            Log.info telemetryLogger
                     ("tx wasn't applied as it was stale " % formatHash)
                     (Crypto.fromHashed txHash)
        TxAppliedEvent txHash ->
            Log.debug telemetryLogger
                      ("tx was correctly applied " % formatHash)
                      (Crypto.fromHashed txHash)
        TxsAddedToMempoolEvent txs ->
            let hashes = map (Crypto.fromHashed . Crypto.hash) txs
            in Log.debug telemetryLogger
                         ("txs added to the mempool " % listOf formatHash)
                         hashes
        TxsRemovedFromMempoolEvent txs ->
            let hashes = map (Crypto.fromHashed . Crypto.hash) txs
            in Log.debug telemetryLogger
                         ("txs removed from the mempool " % listOf formatHash)
                         hashes

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


{------------------------------------------------------------------------------
  Utility functions
-------------------------------------------------------------------------------}

-- | Given a 'Format' which knows how to render to a 'Builder' the elements
-- of a list, return a 'Format' which knows how to format the whole list.
listOf :: Format Builder (a -> Builder) -> Format r ([a] -> r)
listOf formatElement = later (F.build . map (bprint formatElement))
