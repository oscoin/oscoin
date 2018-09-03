module Oscoin.Test.API where

import           Oscoin.Prelude

import qualified Oscoin.Node as Node
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Logging as Log
import           Oscoin.Environment (Environment(Testing))
import           Oscoin.Data.Tx (Tx, mkTx)
import           Oscoin.Test.HTTP.Helpers

import           Data.Aeson.Types (emptyArray)
import           Lens.Micro ((^?!))
import           Lens.Micro.Extras (preview)
import           Lens.Micro.Aeson (key, _String, _JSON, nth)
import           Test.Tasty
import           Test.Tasty.HUnit (Assertion, testCase, assertFailure)
import           Test.Tasty.ExpectedFailure (expectFailBecause)


tests :: [TestTree]
tests =
    [ testCase "Smoke test" (smokeTestOscoinAPI cfg)
    , expectFailBecause "Tests not implemented" $
        testGroup "GET /transactions/:hash" $
            [ test "Tx Not Found" getTxNotFound
            , test "Accept JSON"  getTxAcceptJSON
            , test "Accept CBOR"  getTxAcceptCBOR
            ] <&> ($ cfg)
    ]
  where
    cfg = Node.Config
      { Node.cfgEnv         = Testing
      , Node.cfgLogger      = Log.noLogger
      }

test :: TestName -> Session () -> Node.Config DummyTx -> TestTree
test name session cfg = testCase name $ runSession cfg 42 session

smokeTestOscoinAPI :: Node.Config DummyTx -> Assertion
smokeTestOscoinAPI cfg = runSession cfg 42 $ do
    get "/" >>= assertOK

    -- The mempool is empty.
    get "/node/mempool" >>= assertBody emptyArray

    -- Now let's create a transaction message.
    let msg :: ByteString = "<transaction>"

    -- Now generate a key pair and sign the transaction.
    (pubKey, priKey) <- Crypto.generateKeyPair
    msg'             <- Crypto.sign priKey msg

    let tx :: Tx ByteString = mkTx msg' (Crypto.hash pubKey)

    -- Submit the transaction to the mempool.
    resp <- post "/node/mempool" tx ; assertStatus 202 resp

    -- The response is a transaction receipt, with the transaction
    -- id (hash).
    let txId = responseBody resp ^?! key "tx" . _String

    -- Get the mempool once again, make sure the transaction is in there.
    _mp <- responseBody <$> get "/node/mempool"

    preview (nth 0 . _JSON) _mp @?= Just tx

    get ("/node/mempool/" <> txId) >>= assertOK <> assertJSON

getTxNotFound :: Session ()
getTxNotFound = notTested

getTxAcceptJSON :: Session ()
getTxAcceptJSON = notTested

getTxAcceptCBOR :: Session ()
getTxAcceptCBOR = notTested

notTested :: Session ()
notTested = io $ assertFailure "Not tested"
