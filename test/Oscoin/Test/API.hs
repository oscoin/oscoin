module Oscoin.Test.API where

import           Oscoin.Prelude

import qualified Oscoin.Node as Node
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txPubKey)
import           Oscoin.Crypto.Blockchain (Blockchain(..), BlockHash, blocks)
import           Oscoin.Crypto.Blockchain.Block (Block(..), headerHash, blockData)
import           Oscoin.Test.HTTP.Helpers
import           Oscoin.Test.Crypto.Blockchain.Arbitrary
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import qualified Oscoin.API.Types as API
import           Oscoin.API.HTTP.Internal (fromMediaType, MediaType(..))
import           Oscoin.API.HTTP.Response (GetTxResponse(..))

import           Network.HTTP.Types.Status
import qualified Network.Wai.Test as Wai
import           Network.HTTP.Media ((//))

import           Test.QuickCheck (generate)
import           Test.Tasty
import           Test.Tasty.HUnit (testCase, assertFailure)

tests :: [TestTree]
tests =
    [ test "Smoke test" smokeTestOscoinAPI
    , testGroup "GET /transactions/:hash"
        [ testGroup "404 Not Found"
            [ test "Missing transaction" getMissingTransaction ]

        , testGroup "200 OK"
            [ test "Unconfirmed transaction" getUnconfirmedTransaction
            , test "Confirmed transaction" getConfirmedTransaction
            ]
        ]
    , testGroup "POST /transactions"
        [ testGroup "400 Bad Request"
            [ test "Invalid signature" postTransactionWithInvalidSignature]
        ]
    ]
  where
    test name mkTest = testGroup name $ do
        let ctypes  = fromMediaType <$> [ JSON, CBOR ]
        let accepts = ("*" // "*") : ctypes
        codec <- [ newCodec accept content | content <- ctypes, accept <- accepts ]
        [testCase (show codec) $ mkTest codec >>=
            \HTTPTest{..} -> makeNode testState >>= runSession testSession]

data HTTPTest = HTTPTest
    { testState :: NodeState
    , testSession :: Session ()
    }

httpTest :: NodeState -> Session () -> IO HTTPTest
httpTest state sess = pure $ HTTPTest{ testState = state, testSession = sess }

postTransactionWithInvalidSignature :: Codec -> IO HTTPTest
postTransactionWithInvalidSignature codec = httpTest emptyNodeState $ do
    (_, tx) <- io $ genDummyTx
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

    (txHash, tx) <- io $ genDummyTx
    -- Submit the transaction to the mempool.
    post codec "/transactions" tx >>=
        assertStatus accepted202 <>
        assertResultOK Node.Receipt {fromReceipt = Crypto.hash tx}

    -- Get the mempool once again, make sure the transaction is in there.
    get codec "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK [tx]

    getTransactionReturns codec txHash $ unconfirmedGetTxResponse tx

getMissingTransaction :: Codec -> IO HTTPTest
getMissingTransaction codec = httpTest emptyNodeState $ do
    -- Malformed transaction hash returns a 404
    get codec "/transactions/not-a-hash" >>= assertStatus notFound404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get codec ("/transactions/" <> missing) >>= assertStatus notFound404

getConfirmedTransaction :: Codec -> IO HTTPTest
getConfirmedTransaction codec = do
    chain <- generate $ arbitraryValidBlockchain
    let (tx, blockHash, confirmations) = oldestTx chain
    let txHash = decodeUtf8 $ Crypto.toHex $ Crypto.hash tx

    httpTest (nodeState mempty chain) $
        getTransactionReturns codec txHash $ GetTxResponse
            { txHash = Crypto.hash tx
            , txBlockHash = Just $ blockHash
            , txConfirmations = confirmations
            , txPayload = tx
            }

oldestTx :: Blockchain tx s -> (tx, BlockHash, Word64)
oldestTx (blocks -> blks) = head $ do
    (i, blk) <- zip [1..] blks
    tx <- toList $ blockData blk
    pure (tx, headerHash $ blockHeader blk, i)

getUnconfirmedTransaction :: Codec -> IO HTTPTest
getUnconfirmedTransaction codec = do
    (txHash, tx) <- genDummyTx
    httpTest (nodeState [tx] emptyBlockstore) $
        getTransactionReturns codec txHash $ unconfirmedGetTxResponse tx

unconfirmedGetTxResponse :: API.RadTx -> GetTxResponse
unconfirmedGetTxResponse tx = GetTxResponse
    { txHash = Crypto.hash tx
    , txBlockHash = Nothing
    , txConfirmations = 0
    , txPayload = tx
    }

getTransactionReturns :: Codec -> Text -> GetTxResponse -> Wai.Session ()
getTransactionReturns codec txHash expected =
    get codec ("/transactions/" <> txHash) >>=
    assertStatus ok200 <>
    assertResultOK expected

notTested :: IO HTTPTest
notTested = httpTest emptyNodeState $ io $ assertFailure "Not tested"
