module Test.Control.Concurrent.RateLimit (tests) where

import           Prelude

import           Control.Concurrent.Async (concurrently_)
import           Control.Concurrent.RateLimit

import           Control.Monad (replicateM_)
import           Data.IORef

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests = testGroup "Control.Concurrent.RateLimit"
    [ testCase "actions/s" testActionsPerSecond
    , testCase "actions/s concurrent" testActionsPerSecondConcurrent
    ]

testActionsPerSecond :: Assertion
testActionsPerSecond = do
    lmtr <- newRateLimiter 10 10
    cnt  <- newIORef (0 :: Int)
    replicateM_ 11 $
        rateLimited lmtr 1 $ modifyIORef' cnt (+1)
    readIORef cnt >>= (@?= 10)

testActionsPerSecondConcurrent :: Assertion
testActionsPerSecondConcurrent = do
    lmtr <- newRateLimiter 10 10
    cnt  <- newIORef (0 :: Int)
    concurrently_ (go lmtr cnt) (go lmtr cnt)
    readIORef cnt >>= (@?= 10)
  where
    go lmtr ref =
        replicateM_ 6 $
            rateLimited lmtr 1 $ atomicModifyIORef' ref $ \x -> (x + 1, ())
