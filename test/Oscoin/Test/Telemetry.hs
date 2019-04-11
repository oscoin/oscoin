module Oscoin.Test.Telemetry
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Data.Text as T
import           Data.Text.Lazy.Builder (toLazyText)
import           Network.HTTP.Types.Status
import           Oscoin.Telemetry.Metrics
import qualified Oscoin.Telemetry.Metrics.Internal as Internal
import           Oscoin.Telemetry.Middleware

import           Oscoin.Test.Crypto
import qualified Oscoin.Test.HTTP.Helpers as HTTP
import           Test.Tasty
import           Test.Tasty.HUnit.Extended

tests :: forall c. Dict (IsCrypto c) -> [TestTree]
tests d = [ testCase "Histogram works as expected" testHistogram
          , testGroup "Middleware"
              [ testCase "Smoke test" (smokeTestMiddleware d)
              , testCase "Rendering a counter works" testRenderCounter
              , testCase "Rendering multiple label works" testMultipleLabels
              , testCase "Rendering a gauge works"   testRenderGauge
              , testCase "Rendering an histogram works" testRenderHistogram
              ]
          ]

-- | We test the 'Histogram' against the go implementation, using the
-- documented example as a blueprint
-- (See https://godoc.org/github.com/prometheus/client_golang/prometheus#Histogram)
testHistogram :: Assertion
testHistogram = do
    HistogramSample{..} <- readHistogram =<< newExampleHistogram
    hsCount @?= 1000
    hsSum @?= 29969.50000000001
    hsBuckets @?= [
        (20.0, 192)
      , (25.0, 366)
      , (30.0, 501)
      , (35.0, 638)
      , (40.0, 816)
      ]

exampleLabels :: Labels
exampleLabels = labelsFromList [
     ("env", "staging")
   , ("host", "localhost")
  ]

newExampleHistogram :: IO Histogram
newExampleHistogram = do
    let buckets = linearBuckets 20.0 5.0 5
    h <- newHistogram buckets
    forM_ [0 .. 999] $ \(i :: Int) -> do
        let (m :: Double) = 0.1
        let v = fromIntegral (floor (120.0 * sin (fromIntegral i * m)) :: Int) / 10.0
        observeHistogram h (30.0 + v)
    pure h

smokeTestMiddleware :: forall c. Dict (IsCrypto c) -> Assertion
smokeTestMiddleware Dict = HTTP.runSession @c HTTP.emptyNodeState $
    HTTP.get "/metrics"
        >>= HTTP.assertStatus ok200

testRenderCounter :: Assertion
testRenderCounter = do
    let metric = Internal.Metric "foo.bar.baz" exampleLabels
    counter <- newCounter
    addCounter counter 10
    have <- toLazyText <$> renderCounter mempty (metric, counter)
    toS have @?= ("foo_bar_baz{host=\"localhost\",env=\"staging\"} 10\n" :: Text)

testMultipleLabels :: Assertion
testMultipleLabels = do
    store <- newMetricsStore noLabels
    let metric = "http_requests_total"
    forM_ [1 .. 1027] $ \(_ :: Int) -> do
        let l1 = labelsFromList [("code", "200"), ("method", "post")]
        withCounter (Internal.Metric metric l1) store incCounter
    forM_ [1 .. 3] $ \(_ :: Int) -> do
        let l2 = labelsFromList [("code", "400"), ("method", "post")]
        withCounter (Internal.Metric metric l2) store incCounter
    have <- toLazyText <$> toPrometheusExpositionFormat store
    toS have @?= T.unlines [
          "http_requests_total{method=\"post\",code=\"200\"} 1027"
        , "http_requests_total{method=\"post\",code=\"400\"} 3"
        ]

testRenderGauge :: Assertion
testRenderGauge = do
    let metric = Internal.Metric "metric_without_timestamp_and_labels" exampleLabels
    gauge <- newGauge
    addGauge gauge 12
    have <- toLazyText <$> renderGauge mempty (metric, gauge)
    let want = "metric_without_timestamp_and_labels{host=\"localhost\",env=\"staging\"} 12.0\n"
    toS have @?= (want :: Text)

testRenderHistogram :: Assertion
testRenderHistogram = do
    let metric = Internal.Metric "http.request.duration.seconds" exampleLabels
    h <- newExampleHistogram
    res <- toLazyText <$> renderHistogram mempty (metric, h)
    toS res @?= T.unlines [
          "http_request_duration_seconds_bucket{le=\"20.0\",host=\"localhost\",env=\"staging\"} 192.0"
        , "http_request_duration_seconds_bucket{le=\"25.0\",host=\"localhost\",env=\"staging\"} 366.0"
        , "http_request_duration_seconds_bucket{le=\"30.0\",host=\"localhost\",env=\"staging\"} 501.0"
        , "http_request_duration_seconds_bucket{le=\"35.0\",host=\"localhost\",env=\"staging\"} 638.0"
        , "http_request_duration_seconds_bucket{le=\"40.0\",host=\"localhost\",env=\"staging\"} 816.0"
        , "http_request_duration_seconds_bucket{le=\"+Inf\",host=\"localhost\",env=\"staging\"} 1000"
        , "http_request_duration_seconds_sum 29969.50000000001"
        , "http_request_duration_seconds_count 1000"
        ]
