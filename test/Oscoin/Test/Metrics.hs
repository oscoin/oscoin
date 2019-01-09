module Oscoin.Test.Metrics
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Metrics

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: [TestTree]
tests = [ testCase "Histogram works as expected" testHistogram
        ]

-- | We test the 'Histogram' against the go implementation, using the
-- documented example as a blueprint
-- (See https://godoc.org/github.com/prometheus/client_golang/prometheus#Histogram)
testHistogram :: Assertion
testHistogram = do
    let buckets = linearBuckets 20.0 5.0 5
    h <- newHistogram buckets
    forM_ [0 .. 999] $ \(i :: Int) -> do
        let (m :: Double) = 0.1
        let v = fromIntegral (floor (120.0 * sin (fromIntegral i * m)) :: Int) / 10.0
        observeHistogram h (30.0 + v)
    HistogramSample{..} <- readHistogram h
    hsTotal @?= 1000
    hsSum @?= 29969.50000000001
    readBuckets hsBuckets @?= [
        (20.0, 192.0)
      , (25.0, 366.0)
      , (30.0, 501.0)
      , (35.0, 638.0)
      , (40.0, 816.0)
      ]
