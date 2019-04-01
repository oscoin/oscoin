module Oscoin.Telemetry.Metrics
    ( MetricsStore     -- opaque
    , MonotonicCounter -- opaque
    , Gauge            -- opaque
    , Histogram        -- opaque
    , Buckets          -- opaque
    , Metric           -- opaque
    , Labels           -- opaque
    , HistogramSample(..)
    , Action(..)

    -- * Initialisation
    , newMetricsStore
    , forkEkgServer
    , updateMetricsStore

    -- * Working with labels and metrics
    , labelsFromList
    , labelsToList
    , noLabels
    , metricName
    , metricLabels

    -- * Operations on histograms
    , newHistogram
    , readHistogram
    , observeHistogram

    -- * Ready-to-use buckets
    , defaultBuckets
    , linearBuckets

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

    -- * Internal functions (testing only)
    , withCounter
    ) where

import           Oscoin.Prelude

import           Control.Concurrent.STM
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
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
  Operations on labels and metrics
------------------------------------------------------------------------------}

-- | Creates new 'Labels' out of a list of tuples.
labelsFromList :: [(Text, Text)] -> Labels
labelsFromList labels =
    foldl' (\(Labels acc) (k,v) -> Labels $ Set.insert (Label (k,v)) acc)
           mempty
           labels

noLabels :: Labels
noLabels = Labels mempty

labelsToList :: Labels -> [(Text, Text)]
labelsToList = map unlabel . Set.toDescList . getLabels

{------------------------------------------------------------------------------
  Operations on histograms
------------------------------------------------------------------------------}

-- | Creates a new 'Histogram' given the initial 'Buckets'.
newHistogram :: Buckets -> IO Histogram
newHistogram upperBounds =
    Histogram <$> newTVarIO 0.0
              <*> newCounter
              <*> buckets
              <*> pure upperBounds
  where
      buckets :: IO (Map UpperInclusiveBound MonotonicCounter)
      buckets = M.fromList <$>
          forM (Set.toList . fromBuckets $ upperBounds) (\le -> do
              c <- newCounter
              pure (le, c))

-- | Sample the current value out of an 'Histogram'.
readHistogram :: Histogram -> IO HistogramSample
readHistogram Histogram{..} = do
    hsCount       <- readCounter _histCount
    hsSum         <- readTVarIO _histSum
    hsBuckets     <- readCumulative _histBuckets
    pure HistogramSample{..}
  where
      -- Reads all the buckets in a cumulative fashion, similar to what
      -- the Go implementation <https://github.com/prometheus/client_golang/blob/master/prometheus/histogram.go#L352-L359 does>
      readCumulative :: Map UpperInclusiveBound MonotonicCounter
                     -> IO [(UpperInclusiveBound, Int64)]
      readCumulative bucketsMap = do
          let buckets     = M.toList bucketsMap
          sortOn fst . fst <$>
              foldM (\(!acc, cumCount) (le,c) -> do
                       !cumCount' <- (+ cumCount) <$> readCounter c
                       pure ((le, cumCount') : acc, cumCount')
                    ) (mempty, 0) buckets

-- | Observes (i.e. adds) a new value to the histogram.
observeHistogram :: Histogram -> Double -> IO ()
observeHistogram Histogram{..} measure = do
    -- Step 1, search the 'UpperInclusiveBound' this 'measure' falls into
    case lookupBound (Set.toAscList $ fromBuckets _histUpperBounds) of
        Nothing -> pure ()
        Just le ->
            -- Step 2, increment the total number of observations for that
            -- bucket
            incCounter (_histBuckets M.! le)
    -- Step 3, increment the sum and the counts.
    incCounter _histCount
    atomically $ modifyTVar' _histSum (+ measure)
    where
      lookupBound :: [UpperInclusiveBound] -> Maybe UpperInclusiveBound
      lookupBound []     = Nothing
      lookupBound (x:xs) = if measure <= x then Just x else lookupBound xs

{------------------------------------------------------------------------------
  Operations on buckets
------------------------------------------------------------------------------}

-- | 'Buckets' suitable to measure the latency (in seconds) of a network
-- service.  Adapted from the
-- <https://github.com/prometheus/client_golang/blob/master/prometheus/histogram.go#L61 Go client>
defaultBuckets :: Buckets
defaultBuckets = Buckets $
    Set.fromList [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 10, 2.5, 5.0, 10.0]

-- | Creates linear 'Buckets' from a starting value, a step value and a number
-- of buckets to create.
linearBuckets :: Double      -- ^ Starting value
              -> Double      -- ^ Increment for each bucket
              -> Natural     -- ^ Number of buckets to create
              -> Buckets
linearBuckets start step (fromIntegral -> bucketNumber)
  | bucketNumber == 0 = panic "linearBuckets: you must create at least 1 bucket."
  | otherwise =
      Buckets $ Set.fromList [start, start + step .. start + (step * (bucketNumber - 1))]


{------------------------------------------------------------------------------
  Operations on counters
------------------------------------------------------------------------------}

-- | Creates a new 'MonotonicCounter'.
newCounter :: IO MonotonicCounter
newCounter = do
    -- The fact we are using EKG under the hood is purely an implementation
    -- detail the user shouldn't care about.
    ekgCounter <- EKG.Counter.new
    pure MonotonicCounter {
          _counterInternal = ekgCounter
        , incCounter       = EKG.Counter.inc ekgCounter
        , readCounter      = EKG.Counter.read ekgCounter
        , addCounter       = EKG.Counter.add ekgCounter . fromIntegral
    }

{------------------------------------------------------------------------------
  Operations on gauges
------------------------------------------------------------------------------}

-- | Creates a new 'Gauge'.
newGauge :: IO Gauge
newGauge = do
    -- The fact we are using EKG under the hood is purely an implementation
    -- detail the user shouldn't care about.
    ekgGauge <- EKG.Gauge.new
    pure Gauge {
          _gaugeInternal = ekgGauge
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
    let labeled t = Metric t predefinedLabels
    ekgStore <- EKG.newStore
    gcStatsEnabled <- GHC.getRTSStatsEnabled
    predefinedCounters <-
        if not gcStatsEnabled
             then pure mempty
             else
                foldM (mkCounter ekgStore) mempty [
                    (labeled "rts.gc.bytes_allocated"         , fromIntegral . GHC.allocated_bytes)
                  , (labeled "rts.gc.num_gcs"                 , fromIntegral . GHC.gcs)
                  , (labeled "rts.gc.num_bytes_usage_samples" , fromIntegral . GHC.major_gcs)
                  , (labeled "rts.gc.cumulative_bytes_used"   , fromIntegral . GHC.cumulative_live_bytes)
                  , (labeled "rts.gc.bytes_copied"            , fromIntegral . GHC.copied_bytes)
                  , (labeled "rts.gc.mutator_cpu_ms"          , nsToMs . GHC.mutator_cpu_ns)
                  , (labeled "rts.gc.mutator_wall_ms"         , nsToMs . GHC.mutator_elapsed_ns)
                  , (labeled "rts.gc.gc_cpu_ms"               , nsToMs . GHC.gc_cpu_ns)
                  , (labeled "rts.gc.gc_wall_ms"              , nsToMs . GHC.gc_elapsed_ns)
                  , (labeled "rts.gc.cpu_ms"                  , nsToMs . GHC.cpu_ns)
                  , (labeled "rts.gc.wall_ms"                 , nsToMs . GHC.elapsed_ns)
                  ]
    predefinedGauges   <-
        if not gcStatsEnabled
             then pure mempty
             else
               foldM (mkGauge ekgStore) mempty [
                   (labeled "rts.gc.max_bytes_used"           , fromIntegral . GHC.max_live_bytes)
                 , (labeled "rts.gc.current_bytes_used"       , fromIntegral . GHC.gcdetails_live_bytes . GHC.gc)
                 , (labeled "rts.gc.current_bytes_slop"       , fromIntegral . GHC.gcdetails_slop_bytes . GHC.gc)
                 , (labeled "rts.gc.max_bytes_slop"           , fromIntegral . GHC.max_slop_bytes)
                 , (labeled "rts.gc.peak_megabytes_allocated" , fromIntegral . (`quot` (1024*1024)) . GHC.max_mem_in_use_bytes)
                 , (labeled "rts.gc.par_tot_bytes_copied"     , fromIntegral . GHC.par_copied_bytes)
                 , (labeled "rts.gc.par_avg_bytes_copied"     , fromIntegral . GHC.par_copied_bytes)
                 , (labeled "rts.gc.par_max_bytes_copied"     , fromIntegral . GHC.cumulative_par_max_copied_bytes)
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
                -> Map Metric MonotonicCounter
                -> (Metric, GHC.RTSStats -> Int64)
                -> IO (Map Metric MonotonicCounter)
      mkCounter ekgStore !acc (metric, readValue) = do
            c <- newCounter
            let c' = c { readCounter = do
                             val <- readValue <$> GHC.getRTSStats
                             -- Crucially, we also update this counter every time
                             -- we are requested for a new value, as the user won't
                             -- even know this counter exist in the first place and
                             -- thus cannot update it.
                             addCounter c' (fromIntegral val)
                             pure val
                       }
            EKG.registerCounter (metricName metric) (readCounter c') ekgStore
            pure $ M.insert metric c' acc

      mkGauge :: EKG.Store
              -> Map Metric Gauge
              -> (Metric, GHC.RTSStats -> Int64)
              -> IO (Map Metric Gauge)
      mkGauge ekgStore !acc (metric, readValue) = do
            g <- newGauge
            let g' = g { readGauge = do
                             val <- readValue <$> GHC.getRTSStats
                             -- Crucially, we also update this counter every time
                             -- we are requested for a new value, as the user won't
                             -- even know this counter exist in the first place and
                             -- thus cannot update it.
                             setGauge g' (fromIntegral val)
                             pure val
                       }
            EKG.registerGauge (metricName metric) (readGauge g') ekgStore
            pure $ M.insert metric g' acc

-- | Forks a new EKG server and hooks it to the provided @host:port@.
forkEkgServer :: MetricsStore  -- ^ A 'MetricsStore'.
              -> HostName      -- ^ The desired host.
              -> PortNumber    -- ^ The desired port.
              -> IO ThreadId
forkEkgServer (_msEKGStore -> store) host port =
    EKG.serverThreadId <$>
        EKG.forkServerWith store (C8.pack host) (fromIntegral port)

withMetric :: Metric
           -- ^ The metric to lookup.
           -> TVar (Map Metric w)
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
            m <- newMetric
            atomically $ modifyTVar' allMetrics (M.insert metric m)
            action m
        Just m -> action m

-- | Modifies the 'MonotonicCounter' with the supplied action.
-- Creates the counter if the input label doesn't point to an existing
-- counter.
withCounter :: Metric
            -> MetricsStore
            -> (MonotonicCounter -> IO ())
            -> IO ()
withCounter metric MetricsStore{..} action =
    let createAndRegister = do
           c <- newCounter
           EKG.registerCounter (metricName metric) (readCounter c) _msEKGStore
           pure c
        labeled = metric { metricLabels = _msPredefinedLabels <> metricLabels metric }
    in withMetric labeled _msCounters createAndRegister action

-- | Modifies the 'Gauge' with the supplied action.
-- Creates the gauge if the input label doesn't point to an existing
-- one.
withGauge :: Metric
          -> MetricsStore
          -> (Gauge -> IO ())
          -> IO ()
withGauge metric MetricsStore{..} action =
    let createAndRegister = do
           g <- newGauge
           EKG.registerGauge (metricName metric) (readGauge g) _msEKGStore
           pure g
        labeled = metric { metricLabels = _msPredefinedLabels <> metricLabels metric }
    in withMetric labeled _msGauges createAndRegister action

-- | Modifies the 'Histogram with the supplied action.
-- Creates the histogram if the input label doesn't point to an existing
-- one.
withHistogram :: Metric
              -> Buckets
              -> MetricsStore
              -> (Histogram -> IO ())
              -> IO ()
withHistogram metric buckets MetricsStore{..} action =
    -- There is no 'Histogram' type for EKG, so we cannot directly
    -- register it.
    let onCreation = newHistogram buckets
        labeled = metric { metricLabels = _msPredefinedLabels <> metricLabels metric }
    in withMetric labeled _msHistograms onCreation action

-- | Update the 'MetricsStore' with the given 'Action'.
updateMetricsStore :: MetricsStore -> Action -> IO ()
updateMetricsStore metricsStore action = case action of
    CounterIncrease metric extraLabels ->
        withCounter (Metric metric extraLabels) metricsStore incCounter
    CounterAdd metric extraLabels value ->
        withCounter (Metric metric extraLabels) metricsStore (`addCounter` value)
    GaugeIncrease metric extraLabels ->
        withGauge (Metric metric extraLabels) metricsStore incGauge
    GaugeDecrease metric extraLabels ->
        withGauge (Metric metric extraLabels) metricsStore decGauge
    GaugeSet metric extraLabels value ->
        withGauge (Metric metric extraLabels) metricsStore (`setGauge` value)
    GaugeAdd metric extraLabels value ->
        withGauge (Metric metric extraLabels) metricsStore (`addGauge` value)
    HistogramObserve metric extraLabels buckets value ->
        withHistogram (Metric metric extraLabels) buckets metricsStore (`observeHistogram` value)
