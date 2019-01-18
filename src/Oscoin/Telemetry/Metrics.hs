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
    , addCounter
    , readCounter

    -- * Operations on a Gauge
    , newGauge
    , incGauge
    , decGauge
    , setGauge
    , addGauge
    , readGauge

    ) where

import           Oscoin.Prelude

import           Control.Concurrent.STM
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as M
import           GHC.Natural
import qualified GHC.Stats as GHC
import           Network.Socket (HostName, PortNumber)

import           Oscoin.Telemetry.Metrics.Internal
import           Oscoin.Time (milliseconds)

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
  | bucketNumber == 0 = panic "linearBuckets: you must create at least 1 bucket."
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
newCounter labels = do
    -- The fact we are using EKG under the hood is purely an implementation
    -- detail the user shouldn't care about.
    ekgCounter <- EKG.Counter.new
    pure MonotonicCounter {
          _counterInternal = ekgCounter
        , _counterLabels   = labels
        , incCounter       = EKG.Counter.inc ekgCounter
        , readCounter      = EKG.Counter.read ekgCounter
        , addCounter       = EKG.Counter.add ekgCounter . fromIntegral
    }

{------------------------------------------------------------------------------
  Operations on gauges
------------------------------------------------------------------------------}

-- | Creates a new 'Gauge'.
newGauge :: Labels -> IO Gauge
newGauge labels = do
    -- The fact we are using EKG under the hood is purely an implementation
    -- detail the user shouldn't care about.
    ekgGauge <- EKG.Gauge.new
    pure Gauge {
          _gaugeInternal = ekgGauge
        , _gaugeLabels   = labels
        , incGauge       = EKG.Gauge.inc ekgGauge
        , decGauge       = EKG.Gauge.dec ekgGauge
        , setGauge       = EKG.Gauge.set ekgGauge
        , addGauge       = EKG.Gauge.add ekgGauge
        , readGauge      = EKG.Gauge.read ekgGauge
    }

{------------------------------------------------------------------------------
  Operations on the MetricsStore.
------------------------------------------------------------------------------}

-- | Creates a new 'MetricsStore' with a set of predefined system metrics.
newMetricsStore :: Labels -> IO MetricsStore
newMetricsStore predefinedLabels = do
    ekgStore <- EKG.newStore
    gcStatsEnabled <- GHC.getRTSStatsEnabled
    predefinedCounters <-
        if not gcStatsEnabled
             then pure mempty
             else
                foldM (mkCounter ekgStore) mempty [
                    ("rts.gc.bytes_allocated"         , fromIntegral . GHC.allocated_bytes)
                  , ("rts.gc.num_gcs"                 , fromIntegral . GHC.gcs)
                  , ("rts.gc.num_bytes_usage_samples" , fromIntegral . GHC.major_gcs)
                  , ("rts.gc.cumulative_bytes_used"   , fromIntegral . GHC.cumulative_live_bytes)
                  , ("rts.gc.bytes_copied"            , fromIntegral . GHC.copied_bytes)
                  , ("rts.gc.mutator_cpu_ms"          , nsToMs . GHC.mutator_cpu_ns)
                  , ("rts.gc.mutator_wall_ms"         , nsToMs . GHC.mutator_elapsed_ns)
                  , ("rts.gc.gc_cpu_ms"               , nsToMs . GHC.gc_cpu_ns)
                  , ("rts.gc.gc_wall_ms"              , nsToMs . GHC.gc_elapsed_ns)
                  , ("rts.gc.cpu_ms"                  , nsToMs . GHC.cpu_ns)
                  , ("rts.gc.wall_ms"                 , nsToMs . GHC.elapsed_ns)
                  ]
    predefinedGauges   <-
        if not gcStatsEnabled
             then pure mempty
             else
               foldM (mkGauge ekgStore) mempty [
                   ("rts.gc.max_bytes_used"           , fromIntegral . GHC.max_live_bytes)
                 , ("rts.gc.current_bytes_used"       , fromIntegral . GHC.gcdetails_live_bytes . GHC.gc)
                 , ("rts.gc.current_bytes_slop"       , fromIntegral . GHC.gcdetails_slop_bytes . GHC.gc)
                 , ("rts.gc.max_bytes_slop"           , fromIntegral . GHC.max_slop_bytes)
                 , ("rts.gc.peak_megabytes_allocated" , fromIntegral . (`quot` (1024*1024)) . GHC.max_mem_in_use_bytes)
                 , ("rts.gc.par_tot_bytes_copied"     , fromIntegral . GHC.par_copied_bytes)
                 , ("rts.gc.par_avg_bytes_copied"     , fromIntegral . GHC.par_copied_bytes)
                 , ("rts.gc.par_max_bytes_copied"     , fromIntegral . GHC.cumulative_par_max_copied_bytes)
                 ]
    MetricsStore <$> pure ekgStore
                 <*> newTVarIO mempty
                 <*> newTVarIO predefinedCounters
                 <*> newTVarIO predefinedGauges
                 <*> pure predefinedLabels
    where
      -- | Convert nanoseconds to milliseconds.
      nsToMs :: Int64 -> Int64
      nsToMs s = round (realToFrac s / (fromIntegral milliseconds :: Double))

      mkCounter :: EKG.Store
                -> Map Text MonotonicCounter
                -> (Text, GHC.RTSStats -> Int64)
                -> IO (Map Text MonotonicCounter)
      mkCounter ekgStore !acc (metric, readValue) = do
            c <- newCounter predefinedLabels
            let c' = c { readCounter = do
                             val <- readValue <$> GHC.getRTSStats
                             -- Crucially, we also update this counter every time
                             -- we are requested for a new value, as the user won't
                             -- even know this counter exist in the first place and
                             -- thus cannot update it.
                             addCounter c' (fromIntegral val)
                             pure val
                       }
            EKG.registerCounter metric (readCounter c') ekgStore
            pure $ M.insert metric c' acc

      mkGauge :: EKG.Store
              -> Map Text Gauge
              -> (Text, GHC.RTSStats -> Int64)
              -> IO (Map Text Gauge)
      mkGauge ekgStore !acc (metric, readValue) = do
            g <- newGauge predefinedLabels
            EKG.registerGauge metric (readValue <$> GHC.getRTSStats) ekgStore
            pure $ M.insert metric g acc

-- | Forks a new EKG server and hooks it to the provided @host:port@.
forkEkgServer :: MetricsStore  -- ^ A 'MetricsStore'.
              -> HostName      -- ^ The desired host.
              -> PortNumber    -- ^ The desired port.
              -> IO ()
forkEkgServer ms host port = do
    let store = _msEKGStore ms
    void $ EKG.forkServerWith store (C8.pack host) (fromIntegral port)

withMetric :: Text
           -- ^ The metric to lookup.
           -> TVar (Map Text w)
           -- ^ A collection of metrics
           -> IO w
           -- ^ An action to create and register the metric if not there.
           -> (w -> IO ())
           -- ^ An action to update the metric
           -> IO ()
withMetric metric allMetrics newMetric action = do
    mbMetric <- M.lookup metric <$> readTVarIO allMetrics
    case mbMetric of
         Nothing -> do
             c <- newMetric
             atomically $ modifyTVar' allMetrics (M.insert metric c)
             action c
         Just c -> action c

-- | Modifies the 'MonotonicCounter' with the supplied action.
-- Creates the counter if the input label doesn't point to an existing
-- counter.
withCounter :: Text
            -> Labels
            -> MetricsStore
            -> (MonotonicCounter -> IO ())
            -> IO ()
withCounter metric extraLabels MetricsStore{..} action =
    let labels = _msPredefinedLabels <> extraLabels
        createAndRegister = do
           c <- newCounter labels
           EKG.registerCounter metric (readCounter c) _msEKGStore
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
withGauge metric extraLabels MetricsStore{..} action =
    let labels = _msPredefinedLabels <> extraLabels
        createAndRegister = do
           g <- newGauge labels
           EKG.registerGauge metric (readGauge g) _msEKGStore
           pure g
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
withHistogram metric extraLabels defaultBuckets MetricsStore{..} action =
    -- There is no 'Histogram' type for EKG, so we cannot directly
    -- register it.
    let labels = _msPredefinedLabels <> extraLabels
    in withMetric metric _msHistograms (newHistogram labels defaultBuckets) action

-- | Update the 'MetricsStore' with the given 'Action'.
updateMetricsStore :: MetricsStore -> Action -> IO ()
updateMetricsStore metricsStore action = case action of
    CounterIncrease metric extraLabels ->
        withCounter metric extraLabels metricsStore incCounter
    CounterAdd metric extraLabels value ->
        withCounter metric extraLabels metricsStore (`addCounter` value)
    GaugeIncrease metric extraLabels ->
        withGauge metric extraLabels metricsStore incGauge
    GaugeDecrease metric extraLabels ->
        withGauge metric extraLabels metricsStore decGauge
    GaugeSet metric extraLabels value ->
        withGauge metric extraLabels metricsStore (`setGauge` value)
    GaugeAdd metric extraLabels value ->
        withGauge metric extraLabels metricsStore (`addGauge` value)
    HistogramObserve metric extraLabels buckets value ->
        withHistogram metric extraLabels buckets metricsStore (`observeHistogram` value)
