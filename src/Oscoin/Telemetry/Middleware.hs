{- | Simple module to expose a Wai Middleware to deal with the telemetry
     in the HTTP API.
-}

module Oscoin.Telemetry.Middleware
    ( telemetryMiddleware
      -- * Internal  functions, testing only
    , renderCounter
    , renderGauge
    , renderHistogram
    , toPrometheusExpositionFormat
    ) where

import           Oscoin.Prelude

import           Control.Concurrent.STM
import           Data.Foldable (foldlM)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder, singleton, toLazyText)
import           Formatting
                 ( Format
                 , bprint
                 , build
                 , float
                 , int
                 , mapf
                 , sformat
                 , stext
                 , string
                 , (%)
                 )

import           Oscoin.Telemetry (NotableEvent(..), emit)
import           Oscoin.Telemetry.Internal as Internal
import           Oscoin.Telemetry.Metrics
                 ( labelsFromList
                 , noLabels
                 , readBuckets
                 , readCounter
                 , readGauge
                 , readHistogram
                 )
import           Oscoin.Telemetry.Metrics
import qualified Oscoin.Telemetry.Metrics.Internal as Internal
import           Oscoin.Time (timeDiff)
import qualified Oscoin.Time as Time

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

telemetryMiddleware :: TelemetryStore -> Wai.Middleware
telemetryMiddleware store = loggingMiddleware store
                          . telemetryApiMiddleware store

loggingMiddleware :: TelemetryStore -> Wai.Middleware
loggingMiddleware telemetryStore app req respond = do
    t0 <- Time.now
    app req $ \res -> do
        t1 <- Time.now
        let evt = HttpApiRequest req (Wai.responseStatus res) (t1 `timeDiff` t0)
        emit telemetryStore evt
        respond res

-- | Exposes the 'TelemetryStore' metrics using @Prometheus@ 's
-- < https://prometheus.io/docs/instrumenting/exposition_formats/ exposition format>.
-- However, there is no formal dependency on @Prometheus@ itself as part of this
-- 'Middleware', which can be re-interpreted to be served, say, as JSON.
telemetryApiMiddleware :: TelemetryStore -> Wai.Middleware
telemetryApiMiddleware TelemetryStore{telemetryMetrics} app req respond =
    if     Wai.requestMethod req == HTTP.methodGet
        && Wai.pathInfo req == ["metrics"]
    then respondWithMetrics else app req respond

    where
        respondWithMetrics :: IO Wai.ResponseReceived
        respondWithMetrics = do
            metrics <- toPrometheusExpositionFormat telemetryMetrics
            respond $ Wai.responseLBS HTTP.status200 headers (toS $ toLazyText metrics)
            where
                headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]


-- | Renders the metrics in the exposition format used by Prometheus.
-- TODO(adn): Support 'Metadata' to be attached as labels.
toPrometheusExpositionFormat :: MetricsStore -> IO Builder
toPrometheusExpositionFormat Internal.MetricsStore{..} = do
    (allCounters, allGauges, allHistograms) <- atomically $ do
        c <- M.toList <$> readTVar _msCounters
        g <- M.toList <$> readTVar _msGauges
        h <- M.toList <$> readTVar _msHistograms
        pure (c,g,h)

    countersBuilder   <-
        if null allCounters
           then return mempty
           else foldlM renderCounter mempty allCounters

    gaugesBuilder     <-
        if null allGauges
           then return mempty
           else foldlM renderGauge newline allGauges

    histogramsBuilder <-
        if null allHistograms
           then return mempty
           else foldlM renderHistogram newline allHistograms

    pure $ countersBuilder
        <> gaugesBuilder
        <> histogramsBuilder


newline :: Builder
newline = singleton '\n'

renderCounter :: Builder -> (Metric, MonotonicCounter) -> IO Builder
renderCounter !acc (metric, counter) = do
    sample <- readCounter counter
    let entry = bprint (fmtMetric % " " % int % "\n") metric sample
    pure $ acc <> entry

renderGauge :: Builder -> (Metric, Gauge) -> IO Builder
renderGauge !acc (metric, gauge) = do
    sample <- readGauge gauge
    let entry   = bprint (fmtMetric % " " % float % "\n") metric sample
    pure $ acc <> entry

renderHistogram :: Builder -> (Metric, Histogram) -> IO Builder
renderHistogram !acc (metric, histogram) = do
    sample@HistogramSample{..} <- readHistogram histogram
    let sumMetric = metric { metricName = metricName metric <> "_sum"
                           , metricLabels = noLabels
                           }
    let countMetric = metric { metricName = metricName metric <> "_count"
                             , metricLabels = noLabels
                             }
    pure $ acc
        <> renderBuckets metric sample
        <> bprint (fmtMetric % " " % float % "\n") sumMetric hsSum
        <> bprint (fmtMetric % " " % int % "\n") countMetric hsCount

renderBuckets :: Metric -> HistogramSample -> Builder
renderBuckets metric HistogramSample{..} =
    let userBuckets = foldl' f mempty sortedBuckets
    in userBuckets <> bprint (plusInfBucket % " " % int % "\n") metric hsCount
  where
    f :: Builder -> (Internal.UpperInclusiveBound, Double) -> Builder
    f acc (le, value) =
        let extraLabel = labelsFromList [("le", sformat float le)]
        in  acc <> bprint (fmtBucket extraLabel % " " % float) metric value <> newline

    fmtBucket :: Labels -> Format r (Metric -> r)
    fmtBucket extraLabels =
        mapf (\m -> m { metricName = metricName m <> "_bucket"
                      , metricLabels = metricLabels m <> extraLabels
                      }
             ) fmtMetric

    -- Buckets must be sorted in their upper bounds, in
    -- increasing order.
    sortedBuckets :: [(Internal.UpperInclusiveBound, Double)]
    sortedBuckets =
        sortBy (\(le1,_) (le2,_) -> compare le1 le2) . readBuckets $ hsBuckets

    plusInfBucket = fmtBucket (labelsFromList [("le", "+Inf")])

-- | Converts a namespace-based metric name into a format more suitable to
-- @Prometheus@.
toMetricName :: Text -> Text
toMetricName = T.replace "." "_"

-- | Renders a metric with one or more label attached.
--
-- >>> fmtMetric "oscoin.block_mined" [ ("env", "staging")
--                                     , ("size", 100)
--                                     , ("env", "production")
--                                     ]
-- oscoin_block_mined{env="staging",size="100"}
-- oscoin_block_mined{env="production",size="100"}
--
fmtMetric :: Format r (Metric -> r)
fmtMetric = mapf formatMetric build
    where
        formatMetric :: Metric -> Builder
        formatMetric metric =
            if metricLabels metric == mempty
            then bprint stext (toMetricName . metricName $ metric)
            else let out = T.intercalate ","
                         . map (\(k,v) -> sformat (stext % "=" % string) k (show v))
                         . labelsToList
                         . metricLabels
                         $ metric
            in bprint (stext % "{" % stext % "}") (toMetricName . metricName $ metric) out
