module Oscoin.Test.API.HTTP
    ( tests
    ) where

import           Oscoin.Prelude hiding (get, state)

import qualified Oscoin.API.Types as API
import           Oscoin.Crypto.Blockchain (blocks, genesis)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txPubKey)
import           Oscoin.Test.Crypto.Blockchain.Generators
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.HTTP.Helpers
import           Oscoin.Time.Chrono (toNewestFirst)

import           Data.Default (def)

import           Network.HTTP.Types.Status
import           Web.HttpApiData (toUrlPiece)

import           Oscoin.Test.Crypto
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> [TestTree]
tests Dict =
    [ test "Smoke test" (smokeTestOscoinAPI @c)
    , testGroup "POST /transactions"
        [ testGroup "400 Bad Request"
            [ test "Invalid signature" (postTransactionWithInvalidSignature @c)]
        ]
    , testGroup "GET /blocks/:hash"
        [ test "Missing block" (getMissingBlock @c)
        , test "Existing block" (getExistingBlock @c)
        ]
    , testGroup "GET /blockchain/best"
        [ test "No depth specified" (getBestChain @c)
        ]
    ]
  where
    test name mkTest =
        testProperty name $ monadicIO $ do
            HTTPTest{..} <- mkTest
            liftIO $ runSession testState testSession

data HTTPTest c = HTTPTest
    { testState   :: NodeState c
    , testSession :: Session c ()
    }

httpTest :: NodeState c -> Session c () -> IO (HTTPTest c)
httpTest state sess = pure $ HTTPTest{ testState = state, testSession = sess }

postTransactionWithInvalidSignature
    :: forall c. IsCrypto c
    => PropertyM IO (HTTPTest c)
postTransactionWithInvalidSignature = do
    (_, tx) <- genDummyTx @c
    liftIO $ httpTest emptyNodeState $ do
        otherPubKey <- fst <$> Crypto.generateKeyPair
        let tx' = tx { txPubKey = otherPubKey }
        post "/transactions" tx' >>=
            assertResultErr "Invalid transaction signature" <>
            assertStatus badRequest400

smokeTestOscoinAPI
    :: forall c. IsCrypto c
    => PropertyM IO (HTTPTest c)
smokeTestOscoinAPI = do
    (txHash, tx) <- genDummyTx
    liftIO $ httpTest emptyNodeState $ do
        get "/" >>= assertStatus ok200

        -- The mempool is empty.
        get "/transactions" >>=
            assertStatus ok200 <>
            assertResultOK ([] @(API.RadTx c))

        -- Submit the transaction to the mempool.
        post "/transactions" tx >>=
            assertStatus accepted202 <>
            assertResultOK (API.TxSubmitResponse $ Crypto.hash @c tx)

        -- Get the mempool once again, make sure the transaction is in there.
        get "/transactions" >>=
            assertStatus ok200 <>
            assertResultOK [tx]

        getTransactionReturns txHash $ unconfirmedTx tx

unconfirmedTx
    :: IsCrypto c
    => API.RadTx c
    -> API.TxLookupResponse c
unconfirmedTx tx = API.TxLookupResponse
    { txHash = Crypto.hash tx
    , txBlockHash = Nothing
    , txOutput = Nothing
    , txConfirmations = 0
    , txPayload = tx
    }

getTransactionReturns
    :: IsCrypto c
    => Hashed c (API.RadTx c)
    -> API.TxLookupResponse c
    -> Session c ()
getTransactionReturns txHash expected =
    get ("/transactions/" <> toUrlPiece (Crypto.fromHashed txHash)) >>=
    assertStatus ok200 <>
    assertResultOK expected

getMissingBlock :: forall c. IsCrypto c => PropertyM IO (HTTPTest c)
getMissingBlock = liftIO $ httpTest emptyNodeState $ do
    get "/blocks/not-a-hash" >>= assertStatus notFound404

    let missing = toUrlPiece (Crypto.zeroHash :: Crypto.Hash c)
    get ("/blocks/" <> missing) >>= assertStatus notFound404

getExistingBlock :: IsCrypto c => PropertyM IO (HTTPTest c)
getExistingBlock = do
    chain <- pick genBlockchain
    let g = genesis chain
    let blockId = toUrlPiece $ blockHash g

    liftIO $ httpTest (nodeState mempty chain def) $
        get ("/blocks/" <> blockId) >>=
            assertStatus ok200 <>
            assertResultOK g

getBestChain :: IsCrypto c => PropertyM IO (HTTPTest c)
getBestChain = do
    chain <- pick genBlockchain

    liftIO $ httpTest (nodeState mempty chain def) $ do
        get "/blockchain/best?depth=1" >>=
            assertStatus ok200 <>
            assertResultOK (take 1 . toNewestFirst $ blocks chain)

        get "/blockchain/best" >>=
            assertStatus ok200 <>
            assertResultOK (take 3 . toNewestFirst $ blocks chain)
