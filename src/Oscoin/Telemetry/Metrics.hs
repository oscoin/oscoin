module Oscoin.Telemetry.Metrics
    ( MetricsStore     -- opaque
    , MonotonicCounter -- opaque
    , Gauge            -- opaque
    , Histogram        -- opaque
    , Buckets          -- opaque
    , HistogramSample(..)
    , Action(..)

    -- * Initialisation
    , newMetricsStore
    , forkEkgServer
    , updateMetricsStore

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
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as M
import           GHC.Natural
import           Network.Socket (HostName, PortNumber)

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

data Action =
      CounterIncrease  Text
    | CounterAdd       Text Natural
    | GaugeIncrease    Text
    | GaugeAdd         Text Int64
    | HistogramObserve Text Buckets Double
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

{------------------------------------------------------------------------------
  Operations on histograms
------------------------------------------------------------------------------}

-- | Creates a new 'Histogram' given the initial 'Buckets'.
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
        pure (hsSum, hsBuckets)
    pure $ HistogramSample{..}


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
newMetricsStore :: IO MetricsStore
newMetricsStore = MetricsStore <$> EKG.newStore
                               <*> newTVarIO mempty
                               <*> newTVarIO mempty
                               <*> newTVarIO mempty

-- | Forks a new EKG server and hooks it to the provided @host:port@.
forkEkgServer :: MetricsStore  -- ^ A 'MetricsStore'.
              -> HostName      -- ^ The desired host.
              -> PortNumber    -- ^ The desired port.
              -> IO ()
forkEkgServer ms host port = do
    let store = _msEKGStore ms
    EKG.registerGcMetrics store
    void $ EKG.forkServerWith store (C8.pack host) (fromIntegral port)

withMetric :: Text
           -- ^ The label to lookup.
           -> TVar (Map Text w)
           -- ^ A collection of metrics
           -> IO w
           -- ^ An action to create and register the metric if not there.
           -> (w -> IO ())
           -- ^ An action to update the metric
           -> IO ()
withMetric label metrics newMetric action = do
    mbMetric <- M.lookup label <$> atomically (readTVar metrics)
    case mbMetric of
         Nothing -> do
             c <- newMetric
             atomically $ modifyTVar' metrics (M.insert label c)
             action c
         Just c -> action c

-- | Modifies the 'MonotonicCounter' with the supplied action.
-- Creates the counter if the input label doesn't point to an existing
-- counter.
withCounter :: Text
            -> MetricsStore
            -> (MonotonicCounter -> IO ())
            -> IO ()
withCounter label MetricsStore{..} action =
    let createAndRegister = do
            c@(MonotonicCounter ekgCounter) <- newCounter
            EKG.registerCounter label (EKG.Counter.read ekgCounter) _msEKGStore
            pure c
    in withMetric label _msCounters createAndRegister action

-- | Modifies the 'Gauge' with the supplied action.
-- Creates the gauge if the input label doesn't point to an existing
-- one.
withGauge :: Text
          -> MetricsStore
          -> (Gauge -> IO ())
          -> IO ()
withGauge label MetricsStore{..} action =
    let createAndRegister = do
            c@(Gauge ekgGauge) <- newGauge
            EKG.registerGauge label (EKG.Gauge.read ekgGauge) _msEKGStore
            pure c
    in withMetric label _msGauges createAndRegister action

-- | Modifies the 'Histogram with the supplied action.
-- Creates the histogram if the input label doesn't point to an existing
-- one.
withHistogram :: Text
              -> MetricsStore
              -> Buckets
              -> (Histogram -> IO ())
              -> IO ()
withHistogram label MetricsStore{..} defaultBuckets action =
    -- There is no 'Histogram' type for EKG, so we cannot directly
    -- register it.
    withMetric label _msHistograms (newHistogram defaultBuckets) action

-- | Update the 'MetricsStore' with the given 'Action'.
updateMetricsStore :: MetricsStore -> Action -> IO ()
updateMetricsStore metricsStore action = case action of
    CounterIncrease label ->
        withCounter label metricsStore incCounter
    CounterAdd label value -> do
        withCounter label metricsStore (flip addToCounter value)
    GaugeIncrease label -> do
        withGauge label metricsStore incGauge
    GaugeAdd label value -> do
        withGauge label metricsStore (flip addToGauge value)
    HistogramObserve label buckets value -> do
        withHistogram label metricsStore buckets (flip observeHistogram value)
