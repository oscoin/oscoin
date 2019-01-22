module Oscoin.Telemetry.Internal
    ( -- * Internal types
      TelemetryStore(..)
    ) where

import           Lens.Micro
import           Oscoin.Telemetry.Logging
import           Oscoin.Telemetry.Metrics

data TelemetryStore = TelemetryStore {
      telemetryLogger  :: Logger
    , telemetryMetrics :: MetricsStore
    }

instance HasLogger TelemetryStore where
    loggerL = lens telemetryLogger (\s telemetryLogger -> s { telemetryLogger })

