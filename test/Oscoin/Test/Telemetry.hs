module Oscoin.Test.Telemetry
    ( tests
    ) where

import           Oscoin.Prelude

import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text as T
import           Network.HTTP.Types.Status
import           Oscoin.Telemetry.Metrics
import           Oscoin.Telemetry.Middleware

import qualified Oscoin.Test.HTTP.Helpers as HTTP
import           Test.Tasty
import           Test.Tasty.HUnit.Extended

tests :: [TestTree]
tests = [ testCase "Histogram works as expected" testHistogram
        , testGroup "Middleware"
            [ testCase "Smoke test" smokeTestMiddleware
            , testCase "Rendering a counter works" testRenderCounter
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
    readBuckets hsBuckets @?= [
        (20.0, 192.0)
      , (25.0, 366.0)
      , (30.0, 501.0)
      , (35.0, 638.0)
      , (40.0, 816.0)
      ]

exampleLabels :: Labels
exampleLabels = labelsFromList [
     ("env", "staging")
   , ("host", "localhost")
  ]

newExampleHistogram :: IO Histogram
newExampleHistogram = do
    let buckets = linearBuckets 20.0 5.0 5
    h <- newHistogram exampleLabels buckets
    forM_ [0 .. 999] $ \(i :: Int) -> do
        let (m :: Double) = 0.1
        let v = fromIntegral (floor (120.0 * sin (fromIntegral i * m)) :: Int) / 10.0
        observeHistogram h (30.0 + v)
    pure h

smokeTestMiddleware :: Assertion
smokeTestMiddleware = HTTP.runSession HTTP.emptyNodeState $
    HTTP.get (HTTP.newCodec "text/plain" "text/plain") "/metrics"
        >>= HTTP.assertStatus ok200

testRenderCounter :: Assertion
testRenderCounter = do
    let label = "foo.bar.baz"
    counter <- newCounter exampleLabels
    addToCounter counter 10
    have <- toLazyByteString <$> renderCounter mempty (label, counter)
    toS have @?= ("foo_bar_baz{env=\"staging\",host=\"localhost\"} 10\n" :: Text)

testRenderGauge :: Assertion
testRenderGauge = do
    let label = "metric_without_timestamp_and_labels"
    gauge <- newGauge exampleLabels
    addToGauge gauge 12
    have <- toLazyByteString <$> renderGauge mempty (label, gauge)
    let want = "metric_without_timestamp_and_labels{env=\"staging\",host=\"localhost\"} 12.0\n"
    toS have @?= (want :: Text)

testRenderHistogram :: Assertion
testRenderHistogram = do
    let label = "http.request.duration.seconds"
    h <- newExampleHistogram
    res <- toLazyByteString <$> renderHistogram mempty (label, h)
    toS res @?= T.unlines [
          "http_request_duration_seconds_bucket{env=\"staging\",host=\"localhost\",le=\"20.0\"} 192.0"
        , "http_request_duration_seconds_bucket{env=\"staging\",host=\"localhost\",le=\"25.0\"} 366.0"
        , "http_request_duration_seconds_bucket{env=\"staging\",host=\"localhost\",le=\"30.0\"} 501.0"
        , "http_request_duration_seconds_bucket{env=\"staging\",host=\"localhost\",le=\"35.0\"} 638.0"
        , "http_request_duration_seconds_bucket{env=\"staging\",host=\"localhost\",le=\"40.0\"} 816.0"
        , "http_request_duration_seconds_bucket{env=\"staging\",host=\"localhost\",le=\"+Inf\"} 1000"
        , "http_request_duration_seconds_sum 29969.50000000001"
        , "http_request_duration_seconds_count 1000"
        ]
