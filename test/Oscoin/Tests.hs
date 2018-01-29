module Oscoin.Tests where

import Oscoin.Prelude
import Oscoin.Environment
import Oscoin.HTTP (mkMiddleware)

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

import qualified Network.Wai.Test as Wai
import           Web.Spock (spockAsApp)

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase "API" testOscoinAPI ]

runSession :: Wai.Session () -> Assertion
runSession sess =
    spockAsApp (mkMiddleware Testing) >>= Wai.runSession sess

testOscoinAPI :: Assertion
testOscoinAPI = runSession $ do
    pass
