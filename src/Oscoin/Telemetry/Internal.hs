module Oscoin.Telemetry.Internal
    ( -- * Internal types
      TelemetryStore(..)
    ) where

import           Oscoin.Logging
import           Oscoin.Telemetry.Metrics

data TelemetryStore = TelemetryStore {
      telemetryLogger  :: Logger
    , telemetryMetrics :: MetricsStore
    }
