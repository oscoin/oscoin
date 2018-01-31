module Oscoin.Tests where

import           Oscoin.Prelude
import           Oscoin.HTTP.Test.Helpers

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Aeson (object, (.=))

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase "API" testOscoinAPI ]

testOscoinAPI :: Assertion
testOscoinAPI = runSession $ do
    get "/"     >>= assertOK
    get "/orgs" >>= assertBody (object [])

    post "/orgs/acme" (object []) >>= assertStatus 201
    get  "/orgs/acme"             >>= assertBody (object [])

    put "/orgs/acme/data/zod" (object [("name" .= t "zod")]) >>= assertOK
