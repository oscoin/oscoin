module Oscoin.Test.API where

import           Oscoin.Prelude

import qualified Oscoin.Node                   as Node
import qualified Oscoin.Crypto.Hash            as Crypto
import qualified Oscoin.Crypto.PubKey          as Crypto
import qualified Oscoin.Logging                as Log
import           Oscoin.Environment             ( Environment(Testing) )
import           Oscoin.Data.Tx                 ( mkTx )
import           Oscoin.Test.HTTP.Helpers
import qualified Oscoin.API.HTTP.Result        as Result
import           Oscoin.API.HTTP.Internal       ( ContentType(..) )

import           Radicle                       as Rad
import           Network.HTTP.Types.Method      ( StdMethod(..) )

import           Test.Tasty
import           Test.Tasty.HUnit               ( testCase
                                                , assertFailure
                                                )

tests :: [TestTree]
tests =
    [ test "API Smoke test" smokeTestOscoinAPI
    , test "Tx Not Found "  getTxNotFound
    ]
  where
    test   = httpTest cfg codecs
    cfg    = Node.Config {Node.cfgEnv = Testing, Node.cfgLogger = Log.noLogger}
    ctypes = [JSON, CBOR]
    codecs = [ Codec content accept | content <- ctypes, accept <- ctypes ]

httpTest
    :: Node.Config -> [Codec] -> TestName -> (Codec -> Session ()) -> TestTree
httpTest cfg codecs name session = testGroup
    name
    [ testCase (show codec) $ runSession cfg 42 (session codec)
    | codec <- codecs
    ]

smokeTestOscoinAPI :: Codec -> Session ()
smokeTestOscoinAPI codec@(Codec content accept) = do
    get accept "/" >>= assertStatus 200

    -- The mempool is empty.
    get accept "/transactions" >>= assertStatus 200 <> assertBody
        (Result.ok ([] @DummyTx))

    -- Now let's create a transaction message.
    let msg = Rad.String "transaction"

    -- Now generate a key pair and sign the transaction.
    (pubKey, priKey) <- Crypto.generateKeyPair
    msg'             <- Crypto.sign priKey msg

    let tx :: DummyTx = mkTx msg' (Crypto.hash pubKey)
    let txHash        = decodeUtf8 $ Crypto.toHex $ Crypto.hash tx

    -- Submit the transaction to the mempool.
    request POST "/transactions" (codecHeaders codec) (encodeBody content tx)
        >>= assertStatus 202
        <>  assertBody (Result.Ok Node.Receipt {fromReceipt = Crypto.hash tx})

    -- Get the mempool once again, make sure the transaction is in there.
    get accept "/transactions" >>= assertStatus 200 <> assertBody
        (Result.Ok [tx])

    get accept ("/transactions/" <> txHash) >>= assertStatus 200 <> assertBody
        (Result.Ok tx)

getTxNotFound :: Codec -> Session ()
getTxNotFound (Codec _ accept) = do
    -- Malformed transaction hash returns a 404
    get accept "/transactions/not-a-hash" >>= assertStatus 404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get accept ("/transactions/" <> missing) >>= assertStatus 404

notTested :: Session ()
notTested = io $ assertFailure "Not tested"
