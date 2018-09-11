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
import           Network.HTTP.Types.Method (StdMethod(..))
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
    test   = httpTest codecs
    ctypes = [JSON, CBOR]
    codecs = [ Codec content accept | content <- ctypes, accept <- ctypes ]

httpTest :: [Codec] -> TestName -> (Codec -> Session ()) -> TestTree
httpTest codecs name test = testGroup name
    [testCase (show codec) $ makeNode emptyNodeState >>= runSession (test codec) | codec <- codecs ]

smokeTestOscoinAPI :: Codec -> Session ()
smokeTestOscoinAPI codec@(Codec _ accept) = do
    get accept "/" >>= assertStatus ok200

    -- The mempool is empty.
    get accept "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK ([] @API.RadTx)

    (txHash, tx) <- io $ genDummyTx
    -- Submit the transaction to the mempool.
    request POST "/transactions" (codecHeaders codec) (Just tx) >>=
        assertStatus accepted202 <>
        assertResultOK (Node.Receipt {fromReceipt = Crypto.hash tx})

    -- Get the mempool once again, make sure the transaction is in there.
    get accept "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK [tx]

    get accept ("/transactions/" <> txHash) >>=
        assertStatus ok200 <>
        assertResultOK GetTxResponse
            { txHash = Crypto.hash tx
            , txBlockHash = Nothing
            , txConfirmations = 0
            , txPayload = tx
            }

getMissingTransaction :: Codec -> Session ()
getMissingTransaction (Codec _ accept) = do
    -- Malformed transaction hash returns a 404
    get accept "/transactions/not-a-hash" >>= assertStatus notFound404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get accept ("/transactions/" <> missing) >>= assertStatus notFound404

getConfirmedTransaction :: Codec -> Session ()
getConfirmedTransaction _ = notTested

getUnconfirmedTransaction :: Codec -> Session ()
getUnconfirmedTransaction _ = notTested

notTested :: Session ()
notTested = io $ assertFailure "Not tested"

genDummyTx :: IO (Text, API.RadTx)
genDummyTx = do
    msg :: Rad.Value <- generate arbitrary
    (pubKey, priKey) <- Crypto.generateKeyPair
    signed           <- Crypto.sign priKey msg

    let tx :: API.RadTx = mkTx signed (Crypto.hash pubKey)
    let txHash          = decodeUtf8 $ Crypto.toHex $ Crypto.hash tx

    pure $ (txHash, tx)
