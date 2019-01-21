{- | Simple module to expose a Wai Middleware to deal with the telemetry
     in the HTTP API.
-}

module Oscoin.Telemetry.Middleware
    ( telemetryMiddleware
      -- * Internal  functions, testing only
    , renderCounter
    , renderGauge
    , renderHistogram
    ) where

import           Oscoin.Prelude

import           Control.Concurrent.STM
import           Data.ByteString.Builder as BL
import           Data.Foldable (foldlM)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Formatting (float, int, sformat, stext, string, (%))

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
import           Oscoin.Telemetry.Metrics.Internal as Internal

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

telemetryMiddleware :: TelemetryStore -> Wai.Middleware
telemetryMiddleware store = loggingMiddleware store
                          . telemetryApiMiddleware store

loggingMiddleware :: TelemetryStore -> Wai.Middleware
loggingMiddleware telemetryStore app req respond =
    app req $ \res -> do
        emit telemetryStore (HttpApiRequest req (Wai.responseStatus res))
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
            respond $ Wai.responseLBS HTTP.status200 headers (toLazyByteString metrics)
            where
                headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]


-- | Renders the metrics in the exposition format used by Prometheus.
-- TODO(adn): Support 'Metadata' to be attached as labels.
toPrometheusExpositionFormat :: MetricsStore -> IO Builder
toPrometheusExpositionFormat MetricsStore{..} = do
    (allCounters, allGauges, allHistograms) <- atomically $ do
        c <- readTVar _msCounters
        g <- readTVar _msGauges
        h <- readTVar _msHistograms
        pure (c,g,h)

    countersBuilder   <-
      foldlM renderCounter mempty (M.toList allCounters)

    gaugesBuilder     <-
      foldlM renderGauge mempty (M.toList allGauges)

    histogramsBuilder <-
      foldlM renderHistogram mempty (M.toList allHistograms)

    pure $ countersBuilder
        <> newline
        <> gaugesBuilder
        <> newline
        <> histogramsBuilder


toB :: Text -> BL.Builder
toB = BL.byteString . TE.encodeUtf8

newline :: BL.Builder
newline = BL.char8 '\n'

renderCounter :: Builder -> (Text, MonotonicCounter) -> IO Builder
renderCounter !acc (metric, counter) = do
    sample <- readCounter counter
    let labeled = withLabels metric (_counterLabels counter)
        entry = sformat (stext % " " % int % "\n") labeled sample
    pure $ acc <> toB entry

renderGauge :: Builder -> (Text, Gauge) -> IO Builder
renderGauge !acc (metric, gauge) = do
    sample <- readGauge gauge
    let labeled = withLabels metric (_gaugeLabels gauge)
        entry   = sformat (stext % " " % float % "\n") labeled sample
    pure $ acc <> toB entry

renderHistogram :: Builder -> (Text, Histogram) -> IO Builder
renderHistogram !acc (metric, histogram) = do
    sample@HistogramSample{..} <- readHistogram histogram
    let sumMetric   = withLabels (metric <> "_sum") noLabels
    let countMetric = withLabels (metric <> "_count") noLabels
    pure $ acc
        <> renderBuckets metric (_histLabels histogram) sample
        <> toB (
             sformat (stext % " " % float % "\n") sumMetric hsSum
          <> sformat (stext % " " % int % "\n") countMetric hsCount
        )

renderBuckets :: Text -> Labels -> HistogramSample -> Builder
renderBuckets metric labels HistogramSample{..} =
    let b = foldl' (\acc (le,value) ->
              let metric' =
                      withLabels (metric <> "_bucket")
                                 (labels <> labelsFromList [("le", sformat float le)])
              in  acc
               <> toB (sformat (stext % " " % float) metric' value)
               <> BL.char8 '\n'
           )
           mempty
           -- Buckets must be sorted in their upper bounds, in
           -- increasing order.
           (sortBy (\(le1,_) (le2,_) -> compare le1 le2) . readBuckets $ hsBuckets)
        plusInfBucket = withLabels (metric <> "_bucket")
                                   (labels <> labelsFromList [("le", "+Inf")])
    in b <> toB (sformat (stext % " " % int % "\n") plusInfBucket hsCount)

-- | Converts a namespace-based metric name into a format more suitable to
-- @Prometheus@.
toMetricName :: Text -> Text
toMetricName = T.replace "." "_"

-- | Renders a metric with one or more label attached.
--
-- >>> withLabels "oscoin.block_mined" [("env", "staging"), ("size", 100)]
-- oscoin_block_mined{env="staging",size="100"}
--
withLabels :: Text -> Labels -> Text
withLabels metric (Labels labels)
  | M.null labels = toMetricName metric
  | otherwise =
    let out = T.intercalate ","
            . map (\(k,v) -> sformat (stext % "=" % string) k (show v))
            . M.toList
            $ labels
    in sformat (stext % "{" % stext % "}") (toMetricName metric) out
