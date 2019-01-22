module Oscoin.Telemetry.Internal
    ( -- * Internal types
      Handle(..)
    ) where

import           Lens.Micro
import           Oscoin.Telemetry.Logging
import           Oscoin.Telemetry.Metrics

-- | The telemetry stateful 'Handle'.
data Handle = Handle {
      telemetryLogger  :: Logger
    , telemetryMetrics :: MetricsStore
    }

instance HasLogger Handle where
    loggerL = lens telemetryLogger (\s telemetryLogger -> s { telemetryLogger })

