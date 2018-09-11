module Oscoin.Test.API where

import           Oscoin.Prelude

import qualified Oscoin.Node as Node
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (mkTx)
import           Oscoin.Test.HTTP.Helpers
import qualified Oscoin.API.Types as API
import           Oscoin.API.HTTP.Internal (ContentType(..))
import           Oscoin.API.HTTP.Response (GetTxResponse(..))
import           Oscoin.Test.Data.Rad.Arbitrary ( )

import           Radicle as Rad
import           Network.HTTP.Types.Status

import           Test.QuickCheck (generate, arbitrary)
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
    ]
  where
    test name mkTest = testGroup name $ do
        let ctypes = [ JSON, CBOR ]
        codec <- [ Codec content accept | content <- ctypes, accept <- ctypes ]
        let HTTPTest{..} = mkTest codec
        [testCase (show codec) $ makeNode testState >>= runSession testSession]

data HTTPTest = HTTPTest { testState :: NodeState, testSession :: Session ()}

httpTest :: NodeState -> Session () -> HTTPTest
httpTest state sess = HTTPTest{ testState = state, testSession = sess }

smokeTestOscoinAPI :: Codec -> HTTPTest
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

getMissingTransaction :: Codec -> HTTPTest
getMissingTransaction codec = httpTest emptyNodeState $ do
    -- Malformed transaction hash returns a 404
    get codec "/transactions/not-a-hash" >>= assertStatus notFound404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get codec ("/transactions/" <> missing) >>= assertStatus notFound404

getConfirmedTransaction :: Codec -> HTTPTest
getConfirmedTransaction _ = notTested

getUnconfirmedTransaction :: Codec -> HTTPTest
getUnconfirmedTransaction _ = notTested

notTested :: HTTPTest
notTested = httpTest emptyNodeState $ io $ assertFailure "Not tested"

genDummyTx :: IO (Text, API.RadTx)
genDummyTx = do
    msg :: Rad.Value <- generate arbitrary
    (pubKey, priKey) <- Crypto.generateKeyPair
    signed           <- Crypto.sign priKey msg

    let tx :: API.RadTx = mkTx signed (Crypto.hash pubKey)
    let txHash          = decodeUtf8 $ Crypto.toHex $ Crypto.hash tx

    pure $ (txHash, tx)
