module Data.Conduit.Tests (tests) where

import qualified Data.Conduit.Test.Serialise as Serialise
import           Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Conduit" [Serialise.tests]
