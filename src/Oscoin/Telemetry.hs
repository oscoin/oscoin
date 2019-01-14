module Oscoin.Telemetry
    ( -- * Types
      TelemetryStore -- opaque
    , Metadata
    , Value(..)

    -- * API
    , newTelemetryStore
    , emit
    -- * Handy re-exports
    , module Oscoin.Telemetry.Events
    ) where

import           Oscoin.Prelude

import           Oscoin.Logging

import           Oscoin.Telemetry.Events
import           Oscoin.Telemetry.Internal
                 (Metadata, TelemetryStore(..), Value(..))
import           Oscoin.Telemetry.Metrics

{------------------------------------------------------------------------------
  Single, unified API for both logging and metrics recording.
------------------------------------------------------------------------------}

-- | Creates a new 'TelemetryStore' from a 'Logger' and a 'MetricStore'.
newTelemetryStore :: Logger -> MetricsStore -> TelemetryStore
newTelemetryStore = TelemetryStore

-- | Single /facade/ to the telemetry API. Given a 'NotableEvent' and a list
-- of 'Metadata' for that particular event, dispatch the updates to the metrics
-- and logging systems.
---
--- >>> emit telemetryStore BlockMinedEvent [("blockSize", I 100)]
--
-- NOTE(adn): This function for now just updates the 'MetricsStore', but
-- in the future it should also log stuff.
emit :: TelemetryStore -> NotableEvent -> [Metadata] -> IO ()
emit telemetryStore evt metadata =
    case evt of
        BlockMinedEvent   -> handleBlockMinedEvent metadata telemetryStore
        BlockAppliedEvent -> pure ()
        TxSentEvent       -> pure ()
        TxReceivedEvent   -> pure ()

-- | Maps each 'NotableEvent' to a set of 'Action's. The big pattern-matching
-- block is by design. Despite the repetition (once in 'emit' and once in
-- this function) it guides library authors to be reminded of which points they
-- need to modify in the code each time a new 'NotableEvent' is added.
toActions :: NotableEvent -> [Action]
toActions = \case
    BlockMinedEvent -> [
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
-- TODO(adn): Consider the 'Metadata'.
handleBlockMinedEvent :: [Metadata] -> TelemetryStore -> IO ()
handleBlockMinedEvent _metadata TelemetryStore{..} =
    forM_ (toActions BlockMinedEvent) (updateMetricsStore telemetryMetrics)

