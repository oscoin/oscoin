module Control.Concurrent.Tests (tests) where

import qualified Control.Concurrent.Test.RateLimit as RateLimit

import           Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Concurrent" [RateLimit.tests]
