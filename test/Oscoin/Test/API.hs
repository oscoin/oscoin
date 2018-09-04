module Oscoin.Test.API where

import           Oscoin.Prelude

import qualified Oscoin.Node as Node
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Logging as Log
import           Oscoin.Environment (Environment(Testing))
import           Oscoin.Data.Tx (Tx, mkTx)
import qualified Oscoin.HTTP.API.Result as Result
import           Oscoin.Test.HTTP.Helpers

import           Test.Tasty
import           Test.Tasty.HUnit (testCase, assertFailure)
import           Test.Tasty.ExpectedFailure (expectFail)


tests :: [TestTree]
tests =
    [ test "Smoke test" smokeTestOscoinAPI cfg
    , testGroup "GET /transactions/:hash" $
        [ test "Tx Not Found" getTxNotFound
        , expectFail . test "Accept JSON"  getTxAcceptJSON
        , expectFail . test "Accept CBOR"  getTxAcceptCBOR
        ] <&> ($ cfg)
    ]
  where
    cfg = Node.Config
      { Node.cfgEnv         = Testing
      , Node.cfgLogger      = Log.noLogger
      }

test :: TestName -> Session () -> Node.Config DummyTx -> TestTree
test name session cfg = testCase name $ runSession cfg 42 session

smokeTestOscoinAPI :: Session ()
smokeTestOscoinAPI = do
    get "/" >>= assertOK

    -- The mempool is empty.
    get "/transactions" >>= assertBody (Result.ok ())

    -- Now let's create a transaction message.
    let msg :: ByteString = "<transaction>"

    -- Now generate a key pair and sign the transaction.
    (pubKey, priKey) <- Crypto.generateKeyPair
    msg'             <- Crypto.sign priKey msg

    let tx :: Tx ByteString = mkTx msg' (Crypto.hash pubKey)

    -- Submit the transaction to the mempool.
    resp <- post "/transactions" tx ; assertStatus 202 resp

    Result.Ok receipt <- jsonBody resp

    let txHash = decodeUtf8 $ Crypto.toHex $ Node.fromReceipt @DummyTx receipt

    -- Get the mempool once again, make sure the transaction is in there.
    mp <- jsonBody =<< get "/transactions"
    mp @?= Result.ok [tx]

    get ("/transactions/" <> txHash) >>= assertOK <> assertJSON

getTxNotFound :: Session ()
getTxNotFound = do
    -- Malformed transaction hash returns a 404
    get "/transactions/not-a-hash" >>= assertStatus 404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get ("/transactions/" <> missing) >>= assertStatus 404

getTxAcceptJSON :: Session ()
getTxAcceptJSON = notTested

getTxAcceptCBOR :: Session ()
getTxAcceptCBOR = notTested

notTested :: Session ()
notTested = io $ assertFailure "Not tested"
