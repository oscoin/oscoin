module Oscoin.Telemetry
    ( -- * Types
      TelemetryStore -- opaque

    -- * API
    , newTelemetryStore
    , emit
    -- * Handy re-exports
    , module Oscoin.Telemetry.Events
    ) where

import           Oscoin.Prelude

import           GHC.Stack as GHC

import           Oscoin.Crypto.Hash (formatHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Logging as Log
import           Oscoin.Telemetry.Events
import           Oscoin.Telemetry.Internal (TelemetryStore(..))
import           Oscoin.Telemetry.Metrics

{------------------------------------------------------------------------------
  Single, unified API for both logging and metrics recording.
------------------------------------------------------------------------------}

-- | Creates a new 'TelemetryStore' from a 'Logger' and a 'MetricStore'.
newTelemetryStore :: Logger -> MetricsStore -> TelemetryStore
newTelemetryStore = TelemetryStore

-- | Single /facade/ to the telemetry API. Given a 'NotableEvent',
-- dispatch the updates to the metrics and logging systems.
---
--- >>> emit telemetryStore (BlockMinedEvent (blockHash b)
--
emit :: HasCallStack => TelemetryStore -> NotableEvent -> IO ()
emit telemetryStore evt = GHC.withFrozenCallStack $
    case evt of
        BlockMinedEvent blockHash ->
            handleBlockMinedEvent telemetryStore blockHash
        BlockAppliedEvent -> pure ()
        TxSentEvent       -> pure ()
        TxReceivedEvent   -> pure ()

-- | Maps each 'NotableEvent' to a set of 'Action's. The big pattern-matching
-- block is by design. Despite the repetition (once in 'emit' and once in
-- this function) it guides library authors to be reminded of which points they
-- need to modify in the code each time a new 'NotableEvent' is added.
toActions :: NotableEvent -> [Action]
toActions = \case
    BlockMinedEvent _ -> [
        CounterIncrease "oscoin.block_mined.total" noLabels
      ]
    BlockAppliedEvent -> [
     ]
    TxSentEvent -> [
     ]
    TxReceivedEvent -> [
     ]

-- | For a 'BlockMinedEvent', we want to increment the total counter of
-- mined blocks as well as log some interesting info about the input block.
handleBlockMinedEvent :: HasCallStack
                      => TelemetryStore
                      -> Crypto.Hash
                      -> IO ()
handleBlockMinedEvent TelemetryStore{..} bHash = do
    forM_ (toActions (BlockMinedEvent bHash)) (updateMetricsStore telemetryMetrics)
    Log.info telemetryLogger ("mined block " % formatHash) bHash
