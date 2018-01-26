module Oscoin.Tests where

import Oscoin.Prelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase "API" testOscoinAPI ]

testOscoinAPI :: Assertion
testOscoinAPI = pass
