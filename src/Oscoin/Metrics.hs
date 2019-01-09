module Oscoin.Metrics
    ( MetricsStore     -- opaque
    , MonotonicCounter -- opaque
    , Gauge            -- opaque
    , Histogram        -- opaque
    , Buckets          -- opaque
    , HistogramSample(..)

    -- * Initialisation
    , newEmptyMetricsStore
    , forkEkgServer

    -- * Operations on histograms
    , newHistogram
    , readHistogram
    , linearBuckets
    , readBuckets
    , observeHistogram

    -- * Operations on a MonotonicCounter
    , newCounter
    , incCounter
    , addToCounter
    , readCounter

    -- * Operations on a Gauge
    , newGauge
    , incGauge
    , addToGauge
    , readGauge

    ) where

import           Oscoin.Prelude
import qualified Prelude

import           Control.Concurrent.STM
import           Data.ByteString (ByteString)
import           GHC.Natural

import qualified Data.Map.Strict as M

import qualified System.Metrics as EKG
import qualified System.Metrics.Counter as EKG.Counter
import qualified System.Metrics.Gauge as EKG.Gauge
import qualified System.Remote.Monitoring as EKG

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}

-- | A counter which is monotonically increasing by design.
newtype MonotonicCounter = MonotonicCounter EKG.Counter.Counter
newtype Gauge            = Gauge EKG.Gauge.Gauge

-- | An histogram type modeled after Prometheus' one. It allows sampling the
-- phi-percentile for a given  number of buckets.
data Histogram = Histogram {
    _histTotal   :: MonotonicCounter
  , _histCount   :: TVar Double
  , _histBuckets :: TVar Buckets
  }

-- | A \"snapshot\" of an 'Histogram' at a certain point in time.
data HistogramSample = HistogramSample {
    hsTotal   :: Int64
  , hsSum     :: Double
  , hsBuckets :: Buckets
  } deriving (Show, Eq, Ord)


type UpperInclusiveBound = Double

newtype Buckets = Buckets (Map UpperInclusiveBound Double)
    deriving (Show, Eq, Ord)

-- | An opaque 'MetricsStore'.
data MetricsStore event = MetricsStore {
    _msEKGStore       :: EKG.Store
  -- ^ Used to store metric types which have a 1:1 correspondance with EKG
  -- types. Opaque to the user.
  , _msHistogramStore :: TVar (Map event Histogram)
  -- ^ Used to store metric types which does not have a 1:1 EKG correspondence
  -- (like an 'Histogram') but that could potentially be converted to EKG
  -- types with a bit of labour, at the call site.
  }

{------------------------------------------------------------------------------
  Operations on histograms
------------------------------------------------------------------------------}

newHistogram :: Buckets -> IO Histogram
newHistogram buckets =
    Histogram <$> newCounter
              <*> newTVarIO 0.0
              <*> newTVarIO buckets

-- | Sample the current value out of an 'Histogram'.
readHistogram :: Histogram -> IO HistogramSample
readHistogram Histogram{..} = do
    hsTotal       <- readCounter _histTotal
    (hsSum, hsBuckets) <- atomically $ do
        hsSum     <- readTVar _histCount
        hsBuckets <- readTVar _histBuckets
        return (hsSum, hsBuckets)
    return $ HistogramSample{..}

-- | Observes (i.e. adds) a new value to the histogram.
observeHistogram :: Histogram -> Double -> IO ()
observeHistogram Histogram{..} measure = do
    incCounter _histTotal -- Increment the total number of observations
    atomically $ do
        modifyTVar' _histCount (+ measure)
        modifyTVar' _histBuckets updateBuckets
    where
      -- Stolen from <http://hackage.haskell.org/package/prometheus-2.1.0/docs/src/System.Metrics.Prometheus.Metric.Histogram.html#observe>
      updateBuckets :: Buckets -> Buckets
      updateBuckets (Buckets b) = Buckets $ M.mapWithKey updateBucket b
        where updateBucket key val = bool val (val + 1.0) (measure <= key)

{------------------------------------------------------------------------------
  Operations on buckets
------------------------------------------------------------------------------}

-- | Creates linear 'Buckets' from a starting value, a step value and a number
-- of buckets to create.
linearBuckets :: Double      -- ^ Starting value
              -> Double      -- ^ Increment for each bucket
              -> Natural     -- ^ Number of buckets to create
              -> Buckets
linearBuckets start step (fromIntegral -> bucketNumber)
  | bucketNumber == 0 = Prelude.error "linearBuckets: you must create at least 1 bucket."
  | otherwise = Buckets $
      foldl' (\acc k -> M.insert k 0.0 acc)
             mempty
             [start, start + step .. start + (step * (bucketNumber - 1))]


-- | Read the values of the 'Buckets' at the given point in time.
readBuckets :: Buckets -> [(UpperInclusiveBound, Double)]
readBuckets (Buckets buckets) = M.toList buckets

{------------------------------------------------------------------------------
  Operations on counters
------------------------------------------------------------------------------}

-- | Creates a new 'MonotonicCounter'.
newCounter :: IO MonotonicCounter
newCounter = MonotonicCounter <$> EKG.Counter.new

-- | Reads the current value of a 'MonotonicCounter'.
readCounter :: MonotonicCounter -> IO Int64
readCounter (MonotonicCounter ekgCounter) = EKG.Counter.read ekgCounter

-- | Increase the value of the 'MonotonicCounter' by one.
incCounter :: MonotonicCounter -> IO ()
incCounter (MonotonicCounter ekgCounter) = EKG.Counter.inc ekgCounter

-- | Adds 'x' to the 'MonotonicCounter'.
addToCounter :: MonotonicCounter -> Natural -> IO ()
addToCounter (MonotonicCounter ekgCounter) x =
    EKG.Counter.add ekgCounter (fromIntegral x)

{------------------------------------------------------------------------------
  Operations on gauges
------------------------------------------------------------------------------}

-- | Creates a new 'Gauge'.
newGauge :: IO Gauge
newGauge = Gauge <$> EKG.Gauge.new

-- | Reads the current value of a 'Gauge'.
readGauge :: Gauge -> IO Int64
readGauge (Gauge ekgGauge) = EKG.Gauge.read ekgGauge

-- | Increase the value of the 'Gauge' by one.
incGauge :: Gauge -> IO ()
incGauge (Gauge ekgGauge) = EKG.Gauge.inc ekgGauge

-- | Adds 'x' to the 'Gauge'.
addToGauge :: Gauge -> Int64 -> IO ()
addToGauge (Gauge ekgGauge) = EKG.Gauge.add ekgGauge

{------------------------------------------------------------------------------
  Operations on the MetricsStore.
------------------------------------------------------------------------------}

-- | Creates a new, empty 'MetricsStore'.
newEmptyMetricsStore :: Ord e => IO (MetricsStore e)
newEmptyMetricsStore = MetricsStore <$> EKG.newStore
                                    <*> newTVarIO mempty

-- | Forks a new EKG server and hooks it to the provided @host:port@.
forkEkgServer :: MetricsStore e  -- ^ A 'MetricsStore'.
              -> ByteString      -- ^ The desired host.
              -> Int             -- ^ The desired port.
              -> IO ()
forkEkgServer ms host port =
    void $ EKG.forkServerWith (_msEKGStore ms) host port
