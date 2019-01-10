module Oscoin.Telemetry
    ( -- * Types
      TelemetryStore -- opaque
    , Metadata(..)
    , Value(..)

    -- * API
    , newTelemetryStore
    , emit
    -- * Handy re-exports
    , module Oscoin.Telemetry.Events
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Logging

import           Oscoin.Telemetry.Events
import           Oscoin.Telemetry.Metrics

{------------------------------------------------------------------------------
  Single, unified API for both logging and metrics recording.
------------------------------------------------------------------------------}

data TelemetryStore = TelemetryStore {
      telemetryLogger  :: Logger
    , telemetryMetrics :: MetricsStore
    }

-- | Creates a new 'TelemetryStore' from a 'Logger' and a 'MetricStore'.
newTelemetryStore :: Logger -> MetricsStore -> TelemetryStore
newTelemetryStore = TelemetryStore

-- TODO(adn): Idea -- should a 'NotableEvent' produce a set of interesting
-- 'Action's we want to log?
-- Basically introduce the function @toActions :: NotableEvent -> [Action]@
-- which perhaps could take as input the metadata.
-- We would now remove the @[Action]@ from the type signature of 'emit'
-- which would look like:
--
-- emit :: NotableEvent -> [Metadata] -> TelemetryStore -> IO ()
--
-- a bit like Kim envisioned.

data Metadata = Text :=> Value

data Value = I Int64
           | D Double
           | T Text
           | H Crypto.Hash


-- | Single /facade/ to the telemetry API. Given a 'NotableEvent' and a list
-- of 'Metadata' for that particular event, dispatch the updates to the metrics
-- and logging systems.
---
--- >>> emit BlockMinedEvent ["blockSize" :=> I 100] telemetryStore
--
-- NOTE(adn): This function for now just updates the 'MetricsStore', but
-- in the future it should also log stuff.
emit :: TelemetryStore -> NotableEvent -> [Metadata] -> IO ()
emit telemetryStore evt metadata = do
    -- forM_ actions (updateMetricsStore telemetryMetrics)
    case evt of
        BlockMinedEvent   -> handleBlockMinedEvent metadata telemetryStore
        BlockAppliedEvent -> return ()
        TxSentEvent       -> return ()
        TxReceivedEvent   -> return ()

toActions :: NotableEvent -> [Action]
toActions = \case
    BlockMinedEvent -> [
        CounterIncrease "oscoin.blockMined.num"
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
handleBlockMinedEvent _metadata TelemetryStore{..} = do
    forM_ (toActions BlockMinedEvent) (updateMetricsStore telemetryMetrics)

