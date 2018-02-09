module Oscoin.Tests where

import           Oscoin.Prelude
import           Oscoin.HTTP.Test.Helpers
import           Oscoin.Org (Org(..), OrgTree, mkOrgPath, mkOrgDataPath)
import qualified Oscoin.Org as Org
import qualified Oscoin.Org.Transaction as Org
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Crypto.Hash as Crypto

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Aeson (object, (.=), encode)
import           Data.Aeson.Types (emptyArray)
import qualified Data.Text as T
import           Lens.Micro ((^?))
import           Lens.Micro.Aeson (key, nth, _String)

acme :: Org
acme = Org { orgName = "Acme", orgId = "acme" }

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase "API"     testOscoinAPI
    , testCase "Tx"      testOscoinTxs
    , testCase "Paths"   testOscoinPaths
    , testCase "Crypto"  testOscoinCrypto ]

testOscoinAPI :: Assertion
testOscoinAPI = runSession [("acme", acme)] $ do
    get "/"     >>= assertOK
    get "/orgs" >>= assertBody [acme]

    -- The "acme" org exists.
    get  "/orgs/acme" >>= assertBody acme

    -- The mempool is empty.
    get "/node/mempool" >>= assertBody emptyArray

    -- Now let's create a value we want to store in the org.
    let value = object ["name" .= t "zod"]

    -- Let's create a transaction to store that value under the key
    -- "zod".
    let tx = Org.setTx "acme" "zod" (encode value)

    -- Now generate a key pair and sign the transaction.
    (_, priKey) <- io Crypto.generateKeyPair
    tx'         <- io (Crypto.sign priKey tx)

    -- Submit the transaction to the mempool.
    resp <- post "/node/mempool" tx'
    assertStatus 202 resp

    -- The response is a transaction receipt, with the transaction
    -- id (hash).
    receipt <- jsonBody resp
    let Just txId = receipt ^? key "tx" . _String :: Maybe Text

    -- Get the mempool once again, make sure the transaction is in there.
    mp <- jsonBody =<< get "/node/mempool"
    io $ mp ^? nth 0 . key "id" . _String @?= Just txId

    resp <- get (encodeUtf8 $ "/node/mempool/" <> txId)
    assertOK resp
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

testOscoinCrypto :: Assertion
testOscoinCrypto = do
    let val :: Text = "fnord"
    (_, priKey) <- Crypto.generateKeyPair
    signed <- io $ Crypto.sign priKey val

    -- Verify that hashing a signed message is the same thing as hashing an
    -- unsigned one.
    Crypto.fromHashed (Crypto.hash signed) @?=
        Crypto.fromHashed (Crypto.hash val)

assertValidTx :: HasCallStack => Crypto.Signed Org.Tx -> Assertion
assertValidTx tx =
    case Org.validateTransaction tx of
        Left err ->
            assertFailure (T.unpack $ fromError err)
        _ ->
            pure ()
