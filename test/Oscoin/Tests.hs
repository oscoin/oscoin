module Oscoin.Tests where

import           Oscoin.Prelude

import           Oscoin.Account (Account(..))
import qualified Oscoin.Account as Account
import           Oscoin.Account.Arbitrary ()
import qualified Oscoin.Account.Transaction as Account
import           Oscoin.Crypto.Blockchain (Blockchain(..), validateBlockchain)
import           Oscoin.Crypto.Blockchain.Arbitrary (arbitraryGenesisWith, arbitraryValidBlockWith)
import           Oscoin.Crypto.Blockchain.Block (blockHeader, validateBlock)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.Hash.Arbitrary ()
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Crypto.PubKey.Arbitrary (arbitrarySignedWith)
import           Oscoin.Environment (Environment(Testing))
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Event as Mempool
import           Oscoin.State.Tree (Path, Tree)

import qualified Oscoin.Consensus.Tests as Consensus
import           Oscoin.HTTP.Test.Helpers
import           Oscoin.Test.Helpers

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit hiding ((@?=))
import           Test.Tasty.QuickCheck

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (emptyArray)
import qualified Data.Binary as Binary
import qualified Data.Text as T
import           Lens.Micro ((^?), (^?!))
import           Lens.Micro.Aeson (key, nth, _String)

acme :: Account
acme = Account { accName = "Acme", accId = "acme" }

nodeConfig :: Node.Config
nodeConfig = Node.Config
    { Node.cfgServiceName = "http"
    , Node.cfgPeers       = []
    , Node.cfgEnv         = Testing
    , Node.cfgAccounts    = [("acme", acme)]
    }

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase       "API"                            testOscoinAPI
    , testCase       "Tx"                             testOscoinTxs
    , testCase       "Crypto"                         testOscoinCrypto
    , testCase       "Mempool"                        testOscoinMempool
    , testCase       "Blockchain"                     testOscoinBlockchain
    , testProperty   "Binary instance of Hashed"      propHashedBinary
    , testProperty   "JSON instance of Hashed"        propHashedJSON
    , testProperty   "Hexadecimal encoding"           propHexEncoding
    , testGroup      "Consensus"                      Consensus.tests
    ]

testOscoinAPI :: Assertion
testOscoinAPI = runSession nodeConfig 42 $ do
    get "/"         >>= assertOK
    get "/accounts" >>= assertBody [acme]

    -- The "acme" account exists.
    get  "/accounts/acme" >>= assertBody acme

    -- The mempool is empty.
    get "/node/mempool" >>= assertBody emptyArray

    -- Now let's create a value we want to store in the account.
    let value = Aeson.object ["name" .= t "zod"]

    -- Let's create a transaction to store that value under the key
    -- "zod".
    let tx = Account.setTx "acme" "zod" (Aeson.encode value)

    -- Now generate a key pair and sign the transaction.
    (_, priKey) <- Crypto.generateKeyPair
    tx'         <- Crypto.sign priKey tx

    -- Submit the transaction to the mempool.
    resp <- post "/node/mempool" tx' ; assertStatus 202 resp

    -- The response is a transaction receipt, with the transaction
    -- id (hash).
    let txId = responseBody resp ^?! key "tx" . _String

    -- Get the mempool once again, make sure the transaction is in there.
    mp <- responseBody <$> get "/node/mempool"
    mp ^? nth 0 . key "id" . _String @?= Just txId

    get ("/node/mempool/" <> txId) >>= assertOK <> assertJSON
    get "/accounts/acme/data/doz"  >>= assertStatus 404

    -- TODO: Once we can wait for transactions to be committed, this test
    -- should pass.
    -- ...
    -- Wait for transaction to be committed.
    -- ...
    -- get "/accounts/acme/data/zod" >>= assertBody value

testOscoinTxs :: Assertion
testOscoinTxs = do
    (pubKey, priKey) <- Crypto.generateKeyPair

    -- Create a new, valid `setTx` transaction.
    let tx = Account.setTx "acme" "home" "~"

    -- Sign it and verify it.
    tx' <- Crypto.sign priKey tx
    assertValidTx tx'
    Account.verifySignature pubKey tx' @?= Right tx

    -- Now let's create an empty state tree.
    let tree  = mempty :: Tree Path v

    -- And apply this transaction to it. This should create a key
    -- under `/accounts/acme/data`.
    let tree' = Account.applyTransaction tx tree

    -- The updated state should include the newly set key.
    Account.getPath "acme" ["data", "home"] tree' @?= Just "~"

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
    -- Create a new mempool of account transactions.
    mp <- atomically $ Mempool.new

    -- Create some arbitrary transactions.
    txs <- generate arbitrary :: IO [Account.Tx]

    -- Subscribe to the mempool with the subscription tokens.
    chan1 <- atomically $ Mempool.subscribe mp
    chan2 <- atomically $ Mempool.subscribe mp

    -- Add the transactions.
    atomically $ Mempool.insertMany mp txs

    -- Retrieve all events from the channels.
    evs1 <- atomically $ Mempool.drainChannel chan1
    evs2 <- atomically $ Mempool.drainChannel chan2

    -- Verify that they contain the transactions we generated.
    assertEqual "Chan 1 has all Txs" txs $ concat . mapMaybe fromEvent $ evs1
    assertEqual "Chan 2 has all Txs" txs $ concat . mapMaybe fromEvent $ evs2

    -- If we try to read again, the channel is empty.
    atomically (Mempool.drainChannel chan1) >>= assertEqual "Chan 1 is empty" []
    atomically (Mempool.drainChannel chan2) >>= assertEqual "Chan 2 is empty" []
  where
    fromEvent :: Mempool.Event tx -> Maybe [tx]
    fromEvent (Mempool.Insert txs) = Just txs
    fromEvent _                    = Nothing

testOscoinBlockchain :: Assertion
testOscoinBlockchain = do
    (_, key') <- Crypto.generateKeyPair

    txs <- generate . listOf $
        arbitrarySignedWith key' :: IO [Crypto.Signed Account.Tx]

    gblock <- generate $ arbitraryGenesisWith txs
    assertNoError $ validateBlock gblock

    txs' <- generate . listOf $
        arbitrarySignedWith key' :: IO [Crypto.Signed Account.Tx]

    block <- generate $ arbitraryValidBlockWith (blockHeader gblock) txs'

    assertNoError $ validateBlockchain $ Blockchain (block :| [gblock])

propHashedBinary :: Crypto.Hashed ByteString -> Bool
propHashedBinary x = (Binary.decode . Binary.encode) x == x

propHashedJSON :: Crypto.Hashed ByteString -> Bool
propHashedJSON x = (Aeson.decode . Aeson.encode) x == Just x

propHexEncoding :: ByteString -> Bool
propHexEncoding x = (Crypto.fromHex . Crypto.toHex) x == Right x

assertValidTx :: HasCallStack => Crypto.Signed Account.Tx -> Assertion
assertValidTx tx =
    case Account.validateTransaction tx of
        Left err ->
            assertFailure (T.unpack $ fromError err)
        _ ->
            pure ()
