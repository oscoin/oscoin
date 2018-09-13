module Oscoin.Test.API where

import           Oscoin.Prelude

import qualified Oscoin.Node as Node
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txPubKey)
import           Oscoin.Test.HTTP.Helpers
import qualified Oscoin.API.Types as API
import           Oscoin.API.HTTP.Internal (ContentType(..))
import           Oscoin.API.HTTP.Response (GetTxResponse(..))

import           Network.HTTP.Types.Status

import           Test.Tasty
import           Test.Tasty.ExpectedFailure (expectFail)
import           Test.Tasty.HUnit (testCase, assertFailure)

tests :: [TestTree]
tests =
    [ test "Smoke test" smokeTestOscoinAPI
    , testGroup "GET /transactions/:hash"
        [ testGroup "404 Not Found"
            [ test "Missing transaction" getMissingTransaction ]

        , expectFail $ testGroup "200 OK"
            [ test "Unconfirmed transaction" getUnconfirmedTransaction
            , test "Confirmed transaction"   getConfirmedTransaction
            ]
        ]
    , testGroup "POST /transactions"
        [ testGroup "400 Bad Request"
            [ test "Invalid signature" postTransactionWithInvalidSignature]
        ]
    ]
  where
    test name mkTest = testGroup name $ do
        let ctypes = [ JSON, CBOR ]
        codec <- [ Codec content accept | content <- ctypes, accept <- ctypes ]
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
        assertResultOK (Node.Receipt {fromReceipt = Crypto.hash tx})

    -- Get the mempool once again, make sure the transaction is in there.
    get codec "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK [tx]

    get codec ("/transactions/" <> txHash) >>=
        assertStatus ok200 <>
        assertResultOK GetTxResponse
            { txHash = Crypto.hash tx
            , txBlockHash = Nothing
            , txConfirmations = 0
            , txPayload = tx
            }

getMissingTransaction :: Codec -> IO HTTPTest
getMissingTransaction codec = httpTest emptyNodeState $ do
    -- Malformed transaction hash returns a 404
    get codec "/transactions/not-a-hash" >>= assertStatus notFound404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get codec ("/transactions/" <> missing) >>= assertStatus notFound404

getConfirmedTransaction :: Codec -> IO HTTPTest
getConfirmedTransaction _ = notTested

getUnconfirmedTransaction :: Codec -> IO HTTPTest
getUnconfirmedTransaction _ = notTested

notTested :: IO HTTPTest
notTested = httpTest emptyNodeState $ io $ assertFailure "Not tested"
