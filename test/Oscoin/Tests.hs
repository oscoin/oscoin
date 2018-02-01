module Oscoin.Tests where

import           Oscoin.Prelude
import           Oscoin.HTTP.Test.Helpers
import           Oscoin.Org (Org(..))

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

    let org = Org { orgName = "Acme", orgId = "acme" }
    post "/orgs/acme" org >>= assertStatus 201
    get  "/orgs/acme"     >>= assertBody org

    let value = object ["name" .= t "zod"]
    put "/orgs/acme/data/zod" value >>= assertOK
    get "/orgs/acme/data/zod" >>= assertBody value
