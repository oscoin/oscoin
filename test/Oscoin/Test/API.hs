module Oscoin.Test.API where

import           Oscoin.Prelude

import qualified Oscoin.Node as Node
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Logging as Log
import           Oscoin.Environment (Environment(Testing))
import           Oscoin.Data.Tx (mkTx)
import           Oscoin.Test.HTTP.Helpers
import qualified Oscoin.API.Types as API
import           Oscoin.API.HTTP.Internal (ContentType(..))
import           Oscoin.API.HTTP.Response (GetTxResponse(..))
import           Oscoin.Test.Data.Rad.Arbitrary ( )

import           Control.Monad.Loops (untilJust)
import           Control.Concurrent (threadDelay)
import           Radicle as Rad
import           Network.HTTP.Types.Method (StdMethod(..))
import           Network.HTTP.Types.Status

import           Test.QuickCheck (generate, arbitrary)
import           Test.Tasty
import           Test.Tasty.HUnit (testCase, assertFailure)

tests :: [TestTree]
tests =
    [ test "Smoke test" smokeTestOscoinAPI
    , testGroup "GET /transactions/:hash"
        [ test "404 Not Found" getTransaction404NotFound
        , test "200 OK" getTransaction200OK
        ]
    ]
  where
    test   = httpTest cfg codecs
    cfg    = Node.Config {Node.cfgEnv = Testing, Node.cfgLogger = Log.noLogger}
    ctypes = [JSON, CBOR]
    codecs = [ Codec content accept | content <- ctypes, accept <- ctypes ]

httpTest :: Node.Config -> [Codec] -> TestName -> (Codec -> Session ()) -> TestTree
httpTest cfg codecs name session = testGroup name
    [ testCase (show codec) $ runSession cfg 42 (session codec) | codec <- codecs ]

smokeTestOscoinAPI :: Codec -> Session ()
smokeTestOscoinAPI codec@(Codec _ accept) = do
    get accept "/" >>= assertStatus ok200

    -- The mempool is empty.
    get accept "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK ([] @API.RadTx)

    testSubmittedTxIsConfirmed codec

getTransaction404NotFound :: Codec -> Session ()
getTransaction404NotFound (Codec _ accept) = do
    -- Malformed transaction hash returns a 404
    get accept "/transactions/not-a-hash" >>= assertStatus notFound404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get accept ("/transactions/" <> missing) >>= assertStatus notFound404


getTransaction200OK :: Codec -> Session ()
getTransaction200OK = testSubmittedTxIsConfirmed

testSubmittedTxIsConfirmed :: Codec -> Session ()
testSubmittedTxIsConfirmed codec@(Codec _ accept) = do
    (txId, tx) <- io $ genDummyTx

    request POST "/transactions" (codecHeaders codec) (Just tx) >>=
        assertStatus accepted202 <>
        assertResultOK Node.Receipt{fromReceipt = Crypto.hash tx}

    get accept "/transactions" >>=
        assertStatus ok200 <>
        assertResultOKIncludes tx

    _ <- untilJust $ do
        resp <- get accept ("/transactions/" <> txId)
        assertStatus ok200 resp
        case responseBodyResultOK resp of
            Left err -> io $ assertFailure $ show err
            Right r  -> do
                check txHash (== Crypto.hash tx) r
                check txPayload (== tx) r
                check txConfirmations (>= 0) r
                io $ threadDelay 500000 -- 500ms
                pure $ txBlockHash r

    pure ()

    where check g = assert (Right . g)

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
