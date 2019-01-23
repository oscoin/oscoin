module Oscoin.Telemetry.Metrics.Internal
    ( MetricsStore(..)
    , MonotonicCounter(..)
    , Metric(..)
    , Label(..)
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

-- | A metric identifies a time serie. When embellished with some 'Labels',
-- it enables a dimensional data model where each combination of labels
-- identifies a different time serie.
data Metric = Metric
    { metricName   :: Text
    , metricLabels :: Labels
    } deriving (Show, Eq, Ord)

-- | A label, a key/value pair.
newtype Label = Label { unlabel :: (Text, Text) }
    deriving (Eq, Ord, Show)

-- | A sorted set of labels.
newtype Labels = Labels { getLabels :: Set Label  }
    deriving (Show, Ord, Eq, Semigroup, Monoid)

-- | A monotonically-increasing counter.
data MonotonicCounter = MonotonicCounter {
      _counterInternal :: !EKG.Counter.Counter
    , incCounter       :: IO ()
    , readCounter      :: IO Int64
    , addCounter       :: Natural -> IO ()
    }

-- | A 'Gauge', a metric type which value can go up and down.
data Gauge  = Gauge {
      _gaugeInternal :: !EKG.Gauge.Gauge
    , incGauge       :: IO ()
    , decGauge       :: IO ()
    , setGauge       :: Int64 -> IO ()
    , addGauge       :: Int64 -> IO ()
    , readGauge      :: IO Int64
    }

-- | An histogram type modeled after Prometheus' one. It allows sampling the
-- phi-percentile for a given  number of buckets.
data Histogram = Histogram {
    _histCount   :: !MonotonicCounter
  , _histSum     :: !(TVar Double)
  , _histBuckets :: !(TVar Buckets)
  }

-- | A \"snapshot\" of an 'Histogram' at a certain point in time.
data HistogramSample = HistogramSample {
    hsCount   :: !Int64
  , hsSum     :: !Double
  , hsBuckets :: !Buckets
  } deriving (Show, Eq, Ord)

type UpperInclusiveBound = Double

newtype Buckets = Buckets (Map UpperInclusiveBound Double)
    deriving (Show, Eq, Ord)

-- | A set of 'Action's that a 'NotableEvent' might trigger.
data Action =
      CounterIncrease  !Text !Labels
    | CounterAdd       !Text !Labels !Natural
    | GaugeIncrease    !Text !Labels
    | GaugeDecrease    !Text !Labels
    | GaugeAdd         !Text !Labels !Int64
    | GaugeSet         !Text !Labels !Int64
    | HistogramObserve !Text !Labels !Buckets !Double
    -- ^ Observe a value for an 'Histogram'. It also passes the
    -- defaults 'Buckets' to be used in case this is the first time we update
    -- this histogram.

-- | An opaque 'MetricsStore'.
data MetricsStore = MetricsStore {
    _msEKGStore         :: !EKG.Store
  -- ^ Used to store metric types which have a 1:1 correspondance with EKG
  -- types and that can be ultimately displayed in the EKG dashboard.
  , _msHistograms       :: !(TVar (Map Metric Histogram))
  -- ^ Used to store 'Histogram' metrics, as well as to retrieve and update them.
  , _msCounters         :: !(TVar (Map Metric MonotonicCounter))
  -- ^ Used to store 'Counter' metrics, as well as to retrieve and update them.
  , _msGauges           :: !(TVar (Map Metric Gauge))
  -- ^ Used to store 'Gauge' metrics, as well as to retrieve and update them.
  , _msPredefinedLabels :: !Labels
  -- ^ A set of predefined (system) labels which will be used for each new
  -- metric. An example of a predefined label would be the environment
  -- (testing, production or development).
  }

