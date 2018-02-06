module Oscoin.Tests where

import           Oscoin.Prelude
import           Oscoin.HTTP.Test.Helpers
import           Oscoin.Org (Org(..), OrgTree, mkOrgPath, mkOrgDataPath)
import qualified Oscoin.Org as Org
import qualified Oscoin.Org.Transaction as Org
import qualified Oscoin.Crypto.PubKey as Crypto

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Aeson (object, (.=))
import           Data.Aeson.Types (emptyArray)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase "API"   testOscoinAPI
    , testCase "Tx"    testOscoinTxs
    , testCase "Paths" testOscoinPaths ]

testOscoinAPI :: Assertion
testOscoinAPI = runSession ["acme"] $ do
    get "/"     >>= assertOK
    get "/orgs" >>= assertBody emptyArray

    let org = Org { orgName = "Acme", orgId = "acme" }
    post "/orgs/acme" org >>= assertStatus 202
    -- ...
    -- Wait for transaction to be committed.
    -- ...
    get  "/orgs/acme"     >>= assertBody org

    let value = object ["name" .= t "zod"]
    put "/orgs/acme/data/zod" value >>= assertStatus 202
    -- ...
    -- Wait for transaction to be committed.
    -- ...
    get "/orgs/acme/data/zod" >>= assertBody value
    get "/orgs/acme/data/doz" >>= assertStatus 404

testOscoinTxs :: Assertion
testOscoinTxs = do
    (pubKey, priKey) <- Crypto.generateKeyPair

    -- Create a new, valid `setTx` transaction.
    let tx = Org.setTx "acme" "home" "~"

    -- Sign it and verify it.
    tx' <- Crypto.sign priKey tx
    assertValidTx tx'
    Org.verifySignature pubKey tx' @?= Right tx

    -- Now let's create an empty state tree.
    let tree  = mempty :: OrgTree

    -- And apply this transaction to it. This should create a key
    -- under `/orgs/acme/data`.
    let tree' = Org.applyTransaction tx tree

    -- The updated state should include the newly set key.
    Org.getPath "acme" ["data", "home"] tree' @?= Just "~"

testOscoinPaths :: Assertion
testOscoinPaths = do
    -- Check that our path creation functions work as expected.
    mkOrgPath     "acme" ["key"] @?= ["orgs", "acme", "key"]
    mkOrgDataPath "acme" ["key"] @?= ["orgs", "acme", "data", "key"]

assertValidTx :: HasCallStack => Crypto.Signed Org.Tx -> Assertion
assertValidTx tx =
    case Org.validateTransaction tx of
        Left err ->
            assertFailure (T.unpack $ fromError err)
        _ ->
            pure ()
