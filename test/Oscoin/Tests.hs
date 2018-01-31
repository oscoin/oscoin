module Oscoin.Tests where

import           Oscoin.Prelude
import           Oscoin.HTTP.Test.Helpers

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Aeson (object, (.=))
import           Data.Aeson.Types (emptyArray)

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase "API" testOscoinAPI ]

testOscoinAPI :: Assertion
testOscoinAPI = runSession $ do
    get "/"     >>= assertOK
    get "/orgs" >>= assertBody emptyArray

    post "/orgs/acme" (object []) >>= assertStatus 201
    get  "/orgs/acme"             >>= assertBody emptyArray

    let value = object ["name" .= t "zod"]
    put "/orgs/acme/data/zod" value >>= assertOK
    get "/orgs/acme/data/zod" >>= assertBody value
