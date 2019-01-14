module Oscoin.Telemetry.Metrics
    ( MetricsStore     -- opaque
    , MonotonicCounter -- opaque
    , Gauge            -- opaque
    , Histogram        -- opaque
    , Buckets          -- opaque
    , Labels           -- opaque
    , HistogramSample(..)
    , Action(..)

    -- * Initialisation
    , newMetricsStore
    , forkEkgServer
    , updateMetricsStore
    , labelsFromList
    , noLabels

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

import           Oscoin.Telemetry.Metrics.Internal

import qualified System.Metrics as EKG
import qualified System.Metrics.Counter as EKG.Counter
import qualified System.Metrics.Gauge as EKG.Gauge
import qualified System.Remote.Monitoring as EKG


{------------------------------------------------------------------------------
  Operations on labels
------------------------------------------------------------------------------}

labelsFromList :: [(Text, Text)] -> Labels
labelsFromList = Labels . M.fromList

noLabels :: Labels
noLabels = Labels mempty

{------------------------------------------------------------------------------
  Operations on histograms
------------------------------------------------------------------------------}

-- | Creates a new 'Histogram' given the initial 'Buckets'.
newHistogram :: Labels -> Buckets -> IO Histogram
newHistogram labels buckets =
    Histogram <$> newCounter noLabels
              <*> newTVarIO 0.0
              <*> newTVarIO buckets
              <*> pure labels


-- | Sample the current value out of an 'Histogram'.
readHistogram :: Histogram -> IO HistogramSample
readHistogram Histogram{..} = do
    hsCount       <- readCounter _histCount
    (hsSum, hsBuckets) <- atomically $ do
        hsSum     <- readTVar _histSum
        hsBuckets <- readTVar _histBuckets
        pure (hsSum, hsBuckets)
    pure $ HistogramSample{..}


-- | Observes (i.e. adds) a new value to the histogram.
observeHistogram :: Histogram -> Double -> IO ()
observeHistogram Histogram{..} measure = do
    incCounter _histCount -- Increment the total number of observations
    atomically $ do
        modifyTVar' _histSum (+ measure)
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
newCounter :: Labels -> IO MonotonicCounter
newCounter labels = MonotonicCounter <$> EKG.Counter.new <*> pure labels

-- | Reads the current value of a 'MonotonicCounter'.
readCounter :: MonotonicCounter -> IO Int64
readCounter (MonotonicCounter ekgCounter _) = EKG.Counter.read ekgCounter

-- | Increase the value of the 'MonotonicCounter' by one.
incCounter :: MonotonicCounter -> IO ()
incCounter (MonotonicCounter ekgCounter _) = EKG.Counter.inc ekgCounter

-- | Adds 'x' to the 'MonotonicCounter'.
addToCounter :: MonotonicCounter -> Natural -> IO ()
addToCounter (MonotonicCounter ekgCounter _) x =
    EKG.Counter.add ekgCounter (fromIntegral x)

{------------------------------------------------------------------------------
  Operations on gauges
------------------------------------------------------------------------------}

-- | Creates a new 'Gauge'.
newGauge :: Labels -> IO Gauge
newGauge labels = Gauge <$> EKG.Gauge.new <*> pure labels

-- | Reads the current value of a 'Gauge'.
readGauge :: Gauge -> IO Int64
readGauge (Gauge ekgGauge _) = EKG.Gauge.read ekgGauge

-- | Increase the value of the 'Gauge' by one.
incGauge :: Gauge -> IO ()
incGauge (Gauge ekgGauge _) = EKG.Gauge.inc ekgGauge

-- | Adds 'x' to the 'Gauge'.
addToGauge :: Gauge -> Int64 -> IO ()
addToGauge (Gauge ekgGauge _) = EKG.Gauge.add ekgGauge

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
            -> Labels
            -- ^ Some stock labels to add if the counter doesn't exist yet.
            -> MetricsStore
            -> (MonotonicCounter -> IO ())
            -> IO ()
withCounter metric labels MetricsStore{..} action =
    let createAndRegister = do
            c@(MonotonicCounter ekgCounter _) <- newCounter labels
            EKG.registerCounter metric (EKG.Counter.read ekgCounter) _msEKGStore
            pure c
    in withMetric metric _msCounters createAndRegister action

-- | Modifies the 'Gauge' with the supplied action.
-- Creates the gauge if the input label doesn't point to an existing
-- one.
withGauge :: Text
          -> Labels
          -> MetricsStore
          -> (Gauge -> IO ())
          -> IO ()
withGauge metric labels MetricsStore{..} action =
    let createAndRegister = do
            c@(Gauge ekgGauge _) <- newGauge labels
            EKG.registerGauge metric (EKG.Gauge.read ekgGauge) _msEKGStore
            pure c
    in withMetric metric _msGauges createAndRegister action

-- | Modifies the 'Histogram with the supplied action.
-- Creates the histogram if the input label doesn't point to an existing
-- one.
withHistogram :: Text
              -> Labels
              -> Buckets
              -> MetricsStore
              -> (Histogram -> IO ())
              -> IO ()
withHistogram metric labels defaultBuckets MetricsStore{..} action =
    -- There is no 'Histogram' type for EKG, so we cannot directly
    -- register it.
    withMetric metric _msHistograms (newHistogram labels defaultBuckets) action

-- | Update the 'MetricsStore' with the given 'Action'.
updateMetricsStore :: MetricsStore -> Action -> IO ()
updateMetricsStore metricsStore action = case action of
    CounterIncrease metric labels ->
        withCounter metric labels metricsStore incCounter
    CounterAdd metric labels value ->
        withCounter metric labels metricsStore (`addToCounter` value)
    GaugeIncrease metric labels ->
        withGauge metric labels metricsStore incGauge
    GaugeAdd metric labels value ->
        withGauge metric labels metricsStore (`addToGauge` value)
    HistogramObserve metric labels buckets value ->
        withHistogram metric labels buckets metricsStore (`observeHistogram` value)
