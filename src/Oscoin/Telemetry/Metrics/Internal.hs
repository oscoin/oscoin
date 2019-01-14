module Oscoin.Telemetry.Metrics.Internal
    ( MetricsStore(..)
    , MonotonicCounter(..)
    , Labels(..)
    , Gauge(..)
    , Histogram(..)
    , Buckets(..)
    , UpperInclusiveBound
    , HistogramSample(..)
    , Action(..)
    ) where

import           Oscoin.Prelude

import           Control.Concurrent.STM
import           GHC.Natural

import qualified System.Metrics as EKG
import qualified System.Metrics.Counter as EKG.Counter
import qualified System.Metrics.Gauge as EKG.Gauge

{------------------------------------------------------------------------------
  Main (internal) types
------------------------------------------------------------------------------}

newtype Labels = Labels { getLabels :: Map Text Text }
    deriving (Show, Ord, Eq, Semigroup, Monoid)

-- | A monotonically-increasing counter.
data MonotonicCounter = MonotonicCounter !EKG.Counter.Counter !Labels

data Gauge            = Gauge !EKG.Gauge.Gauge !Labels

-- | An histogram type modeled after Prometheus' one. It allows sampling the
-- phi-percentile for a given  number of buckets.
data Histogram = Histogram {
    _histCount   :: MonotonicCounter
  , _histSum     :: TVar Double
  , _histBuckets :: TVar Buckets
  , _histLabels  :: Labels
  }

-- | A \"snapshot\" of an 'Histogram' at a certain point in time.
data HistogramSample = HistogramSample {
    hsCount   :: Int64
  , hsSum     :: Double
  , hsBuckets :: Buckets
  } deriving (Show, Eq, Ord)

type UpperInclusiveBound = Double

newtype Buckets = Buckets (Map UpperInclusiveBound Double)
    deriving (Show, Eq, Ord)

-- | A set of 'Action's that a 'NotableEvent' might trigger.
data Action =
      CounterIncrease  Text Labels
    | CounterAdd       Text Labels Natural
    | GaugeIncrease    Text Labels
    | GaugeAdd         Text Labels Int64
    | HistogramObserve Text Labels Buckets Double
    -- ^ Observe a value for an 'Histogram'. It also passes the
    -- defaults 'Buckets' to be used in case this is the first time we update
    -- this histogram.

-- | An opaque 'MetricsStore'.
data MetricsStore = MetricsStore {
    _msEKGStore   :: EKG.Store
  -- ^ Used to store metric types which have a 1:1 correspondance with EKG
  -- types and that can be ultimately displayed in the EKG dashboard.
  , _msHistograms :: TVar (Map Text Histogram)
  -- ^ Used to store 'Histogram' metrics, as well as to retrieve and update them.
  , _msCounters   :: TVar (Map Text MonotonicCounter)
  -- ^ Used to store 'Counter' metrics, as well as to retrieve and update them.
  , _msGauges     :: TVar (Map Text Gauge)
  -- ^ Used to store 'Gauge' metrics, as well as to retrieve and update them.
  }

