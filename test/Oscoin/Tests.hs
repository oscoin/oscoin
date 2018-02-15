module Oscoin.Tests where

import           Oscoin.Prelude
import           Oscoin.HTTP.Test.Helpers
import           Oscoin.Org (Org(..), OrgTree, mkOrgPath, mkOrgDataPath)
import qualified Oscoin.Org as Org
import qualified Oscoin.Org.Transaction as Org
import           Oscoin.Org.Arbitrary ()
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Crypto.PubKey.Arbitrary (arbitrarySignedWith)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.Hash.Arbitrary ()
import           Oscoin.Crypto.Blockchain.Block (validateBlock)
import           Oscoin.Crypto.Blockchain.Arbitrary (arbitraryGenesisWith)
import qualified Oscoin.Node.State.Mempool as Mempool
import           Oscoin.Node.Channel (Subscription(..), fromEvent)

import           Test.Tasty
import           Test.Tasty.HUnit hiding ((@?=))
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (emptyArray)
import qualified Data.Text as T
import qualified Data.Binary as Binary
import           Lens.Micro ((^?))
import           Lens.Micro.Aeson (key, nth, _String)

acme :: Org
acme = Org { orgName = "Acme", orgId = "acme" }

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase       "API"                            testOscoinAPI
    , testCase       "Tx"                             testOscoinTxs
    , testCase       "Paths"                          testOscoinPaths
    , testCase       "Crypto"                         testOscoinCrypto
    , testCase       "Mempool"                        testOscoinMempool
    , testCase       "Blockchain"                     testOscoinBlockchain
    , testProperty   "Binary instance of Hashed"      propHashedBinary
    , testProperty   "JSON instance of Hashed"        propHashedJSON
    , testProperty   "Hexadecimal encoding"           propHexEncoding
    ]

testOscoinAPI :: Assertion
testOscoinAPI = runSession [("acme", acme)] $ do
    get "/"     >>= assertOK
    get "/orgs" >>= assertBody [acme]

    -- The "acme" org exists.
    get  "/orgs/acme" >>= assertBody acme

    -- The mempool is empty.
    get "/node/mempool" >>= assertBody emptyArray

    -- Now let's create a value we want to store in the org.
    let value = Aeson.object ["name" .= t "zod"]

    -- Let's create a transaction to store that value under the key
    -- "zod".
    let tx = Org.setTx "acme" "zod" (Aeson.encode value)

    -- Now generate a key pair and sign the transaction.
    (_, priKey) <- Crypto.generateKeyPair
    tx'         <- Crypto.sign priKey tx

    -- Submit the transaction to the mempool.
    resp <- post "/node/mempool" tx' ; assertStatus 202 resp

    -- The response is a transaction receipt, with the transaction
    -- id (hash).
    let txId = fromJust $ responseBody resp ^? key "tx" . _String

    -- Get the mempool once again, make sure the transaction is in there.
    mp <- responseBody <$> get "/node/mempool"
    mp ^? nth 0 . key "id" . _String @?= Just txId

    get ("/node/mempool/" <> txId)
        >>= assertOK <> assertJSON
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
    signed <- Crypto.sign priKey val

    -- Verify that hashing a signed message is the same thing as hashing an
    -- unsigned one.
    Crypto.fromHashed (Crypto.hash signed) @?=
        Crypto.fromHashed (Crypto.hash val)

testOscoinMempool :: Assertion
testOscoinMempool = do
    -- Create two subscription tokens.
    let s1 :: Subscription Org.Tx = Subscription "alice"
        s2 :: Subscription Org.Tx = Subscription "bob"

    -- Create a new mempool of org transactions.
    mp <- Mempool.new @Org.Tx

    -- Create some arbitrary transactions.
    txs <- generate arbitrary :: IO [Org.Tx]

    flip runReaderT mp $ do
        -- Subscribe to the mempool with the subscription tokens.
        chan1 <- Mempool.subscribe s1
        chan2 <- Mempool.subscribe s2

        -- Add the transactions.
        Mempool.addTxs txs

        -- Retrieve all events from the channels.
        evs1 <- Mempool.flushChannel chan1
        evs2 <- Mempool.flushChannel chan2

        -- Verify that they contain the transactions we generated.
        map fromEvent evs1 @?= txs
        map fromEvent evs2 @?= txs

        -- If we try to read again, the channel is empty.
        evs <- Mempool.flushChannel chan1
        evs @?= []

testOscoinBlockchain :: Assertion
testOscoinBlockchain = do
    (_, key') <- Crypto.generateKeyPair

    txs <- generate . listOf $
        arbitrarySignedWith key' :: IO [Crypto.Signed Org.Tx]

    genesis <- generate $ arbitraryGenesisWith txs
    isRight (validateBlock genesis) @? "Block is valid"

propHashedBinary :: Crypto.Hashed ByteString -> Bool
propHashedBinary x = (Binary.decode . Binary.encode) x == x

propHashedJSON :: Crypto.Hashed ByteString -> Bool
propHashedJSON x = (Aeson.decode . Aeson.encode) x == Just x

propHexEncoding :: ByteString -> Bool
propHexEncoding x = (Crypto.fromHex . Crypto.toHex) x == Right x

assertValidTx :: HasCallStack => Crypto.Signed Org.Tx -> Assertion
assertValidTx tx =
    case Org.validateTransaction tx of
        Left err ->
            assertFailure (T.unpack $ fromError err)
        _ ->
            pure ()
