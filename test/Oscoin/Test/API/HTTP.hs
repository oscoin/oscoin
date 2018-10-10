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

import qualified Data.Aeson as Aeson
import           Data.Default (def)
import qualified Data.Text as T

import           Network.HTTP.Media ((//))
import           Network.HTTP.Types.Status
import           Web.HttpApiData (toUrlPiece)

import           Test.QuickCheck (generate)
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ test "Smoke test" smokeTestOscoinAPI
    , testProperty "JSON encoding of Tx" encodeDecodeTx
    , testGroup "POST /transactions"
        [ testGroup "400 Bad Request"
            [ test "Invalid signature" postTransactionWithInvalidSignature]
        ]
    , testGroup "GET /blocks/:hash"
        [ test "Missing block" getMissingBlock
        , test "Existing block" getExistingBlock
        ]
    , testGroup "GET /blockchain/best"
        [ test "No depth specified" getBestChain
        ]
    ]
  where
    test name mkTest = testGroup name $ do
        let ctypes  = fromMediaType <$> [ JSON, CBOR ]
        let accepts = ("*" // "*") : ctypes
        codec <- [ newCodec accept content | content <- ctypes, accept <- accepts ]
        [testCase (T.unpack $ prettyCodec codec) $ mkTest codec >>=
            \HTTPTest{..} -> runSession testState testSession]

data HTTPTest = HTTPTest
    { testState   :: NodeState
    , testSession :: Session ()
    }

httpTest :: NodeState -> Session () -> IO HTTPTest
httpTest state sess = pure $ HTTPTest{ testState = state, testSession = sess }

postTransactionWithInvalidSignature :: Codec -> IO HTTPTest
postTransactionWithInvalidSignature codec = httpTest emptyNodeState $ do
    (_, tx) <- liftIO $ genDummyTx
    otherPubKey <- fst <$> Crypto.generateKeyPair
    let tx' = tx { txPubKey = otherPubKey }
    post codec "/transactions" tx' >>=
        assertResultErr "Invalid transaction signature" <>
        assertStatus badRequest400

smokeTestOscoinAPI :: Codec -> IO HTTPTest
smokeTestOscoinAPI codec = httpTest emptyNodeState $ do
    get codec "/" >>= assertStatus ok200

    -- The mempool is empty.
    get codec "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK ([] @API.RadTx)

    (txHash, tx) <- liftIO $ genDummyTx
    -- Submit the transaction to the mempool.
    post codec "/transactions" tx >>=
        assertStatus accepted202 <>
        assertResultOK (API.TxSubmitResponse $ Crypto.hash tx)

    -- Get the mempool once again, make sure the transaction is in there.
    get codec "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK [tx]

    getTransactionReturns codec txHash $ unconfirmedTx tx

unconfirmedTx :: API.RadTx -> API.TxLookupResponse
unconfirmedTx tx = API.TxLookupResponse
    { txHash = Crypto.hash tx
    , txBlockHash = Nothing
    , txOutput = Nothing
    , txConfirmations = 0
    , txPayload = tx
    }

getTransactionReturns :: Codec -> Hashed API.RadTx -> API.TxLookupResponse -> Session ()
getTransactionReturns codec txHash expected =
    get codec ("/transactions/" <> toUrlPiece (Crypto.fromHashed txHash)) >>=
    assertStatus ok200 <>
    assertResultOK expected

getMissingBlock :: Codec -> IO HTTPTest
getMissingBlock codec = httpTest emptyNodeState $ do
    get codec "/blocks/not-a-hash" >>= assertStatus notFound404

    let missing = toUrlPiece (Crypto.zeroHash :: Crypto.Hash)
    get codec ("/blocks/" <> missing) >>= assertStatus notFound404

getExistingBlock :: Codec -> IO HTTPTest
getExistingBlock codec = do
    chain <- generate $ arbitraryValidBlockchain def
    let g = void $ genesis chain
    let blockId = toUrlPiece . Crypto.fromHashed $ blockHash g

    httpTest (nodeState mempty chain) $
        get codec ("/blocks/" <> blockId) >>=
            assertStatus ok200 <>
            assertResultOK g

getBestChain :: Codec -> IO HTTPTest
getBestChain codec = do
    chain <- generate $ arbitraryValidBlockchain def

    httpTest (nodeState mempty chain) $ do
        get codec "/blockchain/best?depth=1" >>=
            assertStatus ok200 <>
            assertResultOK (map void $ take 1 $ blocks chain)

        get codec "/blockchain/best" >>=
            assertStatus ok200 <>
            assertResultOK (map void $ take 3 $ blocks chain)

encodeDecodeTx :: API.RadTx -> Bool
encodeDecodeTx tx = (Aeson.decode . Aeson.encode) tx == Just tx && verifyTx tx
