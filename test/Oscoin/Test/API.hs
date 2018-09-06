module Oscoin.Test.API where

import           Oscoin.Prelude

import qualified Oscoin.Node as Node
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Logging as Log
import           Oscoin.Environment (Environment(Testing))
import           Oscoin.Data.Tx (mkTx)
import           Oscoin.Test.HTTP.Helpers
import qualified Oscoin.HTTP.API.Result as Result
import           Oscoin.HTTP.Internal (ContentType(..))

import           Radicle as Rad
import           Network.HTTP.Types.Method (StdMethod(..))

import           Test.Tasty
import           Test.Tasty.HUnit (testCase, assertFailure)

tests :: [TestTree]
tests = do
    content <- [JSON, CBOR]
    accept  <- [JSON, CBOR]

    let codec = Codec content accept
    let cfg = Node.Config {Node.cfgEnv = Testing, Node.cfgLogger = Log.noLogger}

    [ test cfg ("API Smoke test " ++ show codec) (smokeTestOscoinAPI codec),
      test cfg ("Tx Not Found " ++ show codec) (getTxNotFound codec) ]

test :: Node.Config -> TestName -> Session () -> TestTree
test cfg name session = testCase name $ runSession cfg 42 session

smokeTestOscoinAPI :: Codec -> Session ()
smokeTestOscoinAPI codec@(Codec content accept) = do
    get accept "/" >>= assertStatus 200

    -- The mempool is empty.
    txs <- get accept "/transactions"
    assertStatus 200 txs; assertBody (Result.ok ([] :: [DummyTx])) txs

    -- Now let's create a transaction message.
    let msg = Rad.String "transaction"

    -- Now generate a key pair and sign the transaction.
    (pubKey, priKey) <- Crypto.generateKeyPair
    msg'             <- Crypto.sign priKey msg

    let tx :: DummyTx = mkTx msg' (Crypto.hash pubKey)
    let txHash = decodeUtf8 $ Crypto.toHex $ Crypto.hash tx

    -- Submit the transaction to the mempool.
    tx' <- request POST "/transactions" (codecHeaders codec) (encodeBody content tx)
    assertStatus 202 tx'
    assertBody (Result.Ok Node.Receipt{fromReceipt = Crypto.hash tx}) tx'

    -- Get the mempool once again, make sure the transaction is in there.
    txs' <- get accept "/transactions"
    assertStatus 200 txs'; assertBody (Result.Ok [tx]) txs'

    tx'' <- get accept ("/transactions/" <> txHash)
    assertStatus 200 tx''; assertBody (Result.Ok tx) tx''

getTxNotFound :: Codec -> Session ()
getTxNotFound (Codec _ accept) = do
    -- Malformed transaction hash returns a 404
    get accept "/transactions/not-a-hash" >>= assertStatus 404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get accept ("/transactions/" <> missing) >>= assertStatus 404

notTested :: Session ()
notTested = io $ assertFailure "Not tested"
