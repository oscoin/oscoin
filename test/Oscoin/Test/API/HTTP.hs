module Oscoin.Test.API.HTTP
    ( tests
    ) where

import           Oscoin.Prelude hiding (get, state)

import           Oscoin.API.HTTP.Internal (MediaType(..), fromMediaType)
import qualified Oscoin.API.Types as API
import           Oscoin.Crypto.Blockchain (blocks, genesis)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txPubKey, verifyTx)
import           Oscoin.Test.Crypto.Blockchain.Arbitrary
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.HTTP.Helpers
import           Oscoin.Time.Chrono (toNewestFirst)

import qualified Data.Aeson as Aeson
import           Data.Default (def)
import qualified Data.Text as T

import           Network.HTTP.Media ((//))
import           Network.HTTP.Types.Status
import           Web.HttpApiData (toUrlPiece)

import           Oscoin.Test.Crypto
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> [TestTree]
tests Dict =
    [ test "Smoke test" (smokeTestOscoinAPI @c)
    , testProperty "JSON encoding of Tx" (encodeDecodeTx @c)
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
    test name mkTest = testGroup name $ do
        let ctypes  = fromMediaType <$> [ JSON, CBOR ]
        let accepts = ("*" // "*") : ctypes
        codec <- [ newCodec accept content | content <- ctypes, accept <- accepts ]
        [ testProperty (T.unpack $ prettyCodec codec) (
            monadicIO $ do
                HTTPTest{..} <- mkTest codec
                liftIO $ runSession testState testSession )]

data HTTPTest c = HTTPTest
    { testState   :: NodeState c
    , testSession :: Session c ()
    }

httpTest :: NodeState c -> Session c () -> IO (HTTPTest c)
httpTest state sess = pure $ HTTPTest{ testState = state, testSession = sess }

postTransactionWithInvalidSignature
    :: forall c. IsCrypto c
    => Codec
    -> PropertyM IO (HTTPTest c)
postTransactionWithInvalidSignature codec = do
    (_, tx) <- genDummyTx @c
    liftIO $ httpTest emptyNodeState $ do
        otherPubKey <- fst <$> Crypto.generateKeyPair
        let tx' = tx { txPubKey = otherPubKey }
        post codec "/transactions" tx' >>=
            assertResultErr "Invalid transaction signature" <>
            assertStatus badRequest400

smokeTestOscoinAPI
    :: forall c. IsCrypto c
    => Codec
    -> PropertyM IO (HTTPTest c)
smokeTestOscoinAPI codec = do
    (txHash, tx) <- genDummyTx
    liftIO $ httpTest emptyNodeState $ do
        get codec "/" >>= assertStatus ok200

        -- The mempool is empty.
        get codec "/transactions" >>=
            assertStatus ok200 <>
            assertResultOK ([] @(API.RadTx c))

        -- Submit the transaction to the mempool.
        post codec "/transactions" tx >>=
            assertStatus accepted202 <>
            assertResultOK (API.TxSubmitResponse $ Crypto.hash @c tx)

        -- Get the mempool once again, make sure the transaction is in there.
        get codec "/transactions" >>=
            assertStatus ok200 <>
            assertResultOK [tx]

        getTransactionReturns codec txHash $ unconfirmedTx tx

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
    => Codec
    -> Hashed c (API.RadTx c)
    -> API.TxLookupResponse c
    -> Session c ()
getTransactionReturns codec txHash expected =
    get codec ("/transactions/" <> toUrlPiece (Crypto.fromHashed txHash)) >>=
    assertStatus ok200 <>
    assertResultOK expected

getMissingBlock :: forall c. IsCrypto c => Codec -> PropertyM IO (HTTPTest c)
getMissingBlock codec = liftIO $ httpTest emptyNodeState $ do
    get codec "/blocks/not-a-hash" >>= assertStatus notFound404

    let missing = toUrlPiece (Crypto.zeroHash :: Crypto.Hash c)
    get codec ("/blocks/" <> missing) >>= assertStatus notFound404

getExistingBlock :: IsCrypto c => Codec -> PropertyM IO (HTTPTest c)
getExistingBlock codec = do
    chain <- pick arbitraryBlockchain
    let g = genesis chain
    let blockId = toUrlPiece $ blockHash g

    liftIO $ httpTest (nodeState mempty chain def) $
        get codec ("/blocks/" <> blockId) >>=
            assertStatus ok200 <>
            assertResultOK g

getBestChain :: IsCrypto c => Codec -> PropertyM IO (HTTPTest c)
getBestChain codec = do
    chain <- pick arbitraryBlockchain

    liftIO $ httpTest (nodeState mempty chain def) $ do
        get codec "/blockchain/best?depth=1" >>=
            assertStatus ok200 <>
            assertResultOK (take 1 . toNewestFirst $ blocks chain)

        get codec "/blockchain/best" >>=
            assertStatus ok200 <>
            assertResultOK (take 3 . toNewestFirst $ blocks chain)

encodeDecodeTx :: IsCrypto c => API.RadTx c -> Property
encodeDecodeTx tx =
    (Aeson.eitherDecode . Aeson.encode) tx === Right tx .&&. verifyTx tx
