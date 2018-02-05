module Oscoin.Tests where

import           Oscoin.Prelude
import           Oscoin.HTTP.Test.Helpers
import           Oscoin.Org (Org(..))
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Org.Transaction

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Aeson (object, (.=))
import           Data.Aeson.Types (emptyArray)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase "API" testOscoinAPI
    , testCase "Tx"  testOscoinTxs ]

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
    get "/orgs/acme/data/doz" >>= assertStatus 404

testOscoinTxs :: Assertion
testOscoinTxs = do
    (pubKey, priKey) <- Crypto.generateKeyPair

    let tx = setTx "acme" "home" "~"

    signed <- Crypto.sign priKey tx
    assertValidTx signed

    Crypto.verify pubKey signed @? "Signature should verify"

assertValidTx :: HasCallStack => Crypto.Signed Tx -> Assertion
assertValidTx tx =
    case validateTx tx of
        Left err ->
            assertFailure (T.unpack $ fromError err)
        _ ->
            pure ()
