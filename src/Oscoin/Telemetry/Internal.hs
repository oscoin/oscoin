module Oscoin.Telemetry.Internal
    ( -- * Internal types
      TelemetryStore(..)
    , Metadata
    , Key
    , Value(..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Logging

import           Oscoin.Telemetry.Metrics

data TelemetryStore = TelemetryStore {
      telemetryLogger  :: Logger
    , telemetryMetrics :: MetricsStore
    }

type Key = Text

-- | A @(key,value)@ pair mapping.
type Metadata = (Key, Value)

-- | The possible set of values the user might want to pass as metadata.
data Value = I Int64
           | D Double
           | T Text
           | H Crypto.Hash
