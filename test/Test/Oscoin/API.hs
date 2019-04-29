-- | Tests behavior of the Node through its API using a test
-- implementation of 'MonadClient'.
module Test.Oscoin.API
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import           Oscoin.Crypto.Blockchain.Block (blockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.Tx (DummyPayload(..))
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool

import qualified Data.Text as T

import qualified Oscoin.Test.API.HTTP.TestClient as Client
import           Oscoin.Test.Crypto
import           Oscoin.Test.HTTP.Helpers
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck


tests :: forall c. Dict (IsCrypto c) -> TestTree
tests Dict = testGroup "Test.Oscoin.API"
    [ testGroup "getState"
        [ testProperty "existing value" $ monadicIO $ do
            -- #341: We need 'listOf1' here as generating an
            -- empty list will make the test fail as the path would be
            -- empty.
            path <- pick (listOf1 arbitraryIdent)
            txValue <- pick arbitrary
            let bindings = [(T.intercalate "/" path, txValue)]
            liftIO $ runSessionBindings @c bindings $ do
                result <- Client.run (Client.getState (Proxy @c) path)
                result @?= API.Ok txValue

        , testProperty "non-existing value" $ monadicIO $ do
            path <- pick (listOf arbitraryIdent)
            liftIO $ runEmptySession @c $ do
              result <- Client.run (Client.getState (Proxy @c) path)
              result @?= API.Err "Value not found"

        , testProperty "existing reference" $ monadicIO $ do
            refName <- pick arbitraryIdent
            txValue <- pick arbitrary
            let env = initEnv []
                      & addRef refName txValue
            liftIO $ runSessionEnv @c env $ do
                result <- Client.run (Client.getState (Proxy @c) [refName])
                result @?= API.Ok txValue
        ]
    , testGroup "getTransaction" $

        [ testProperty "missing transaction" $ monadicIO $ do
            txHash <- pick (arbitraryHash @c)
            liftIO $ runEmptySession @c $ do
                response <- Client.run (Client.getTransaction txHash)
                response @?= API.Err "Transaction not found"

        , testCase "unconfirmed transaction" $ runEmptySession $ do
            let txValue = DummyPayload "yo"
            (txHash, tx) <- createValidTx @c txValue
            liftNode $ Mempool.addTxs [tx]
            response <- Client.run (Client.getTransaction txHash)
            let expected = API.TxLookupResponse
                    { txHash = Crypto.hash tx
                    , txBlockHash = Nothing
                    , txOutput = Nothing
                    , txConfirmations = 0
                    , txPayload = tx
                    }
            response @?= API.Ok expected

        , testCase "confirmed transaction" $ runEmptySession @c $ do
            let txValue = DummyPayload "yo"
            (txHash, tx) <- createValidTx txValue
            Just blk <- liftNode $ do
                Mempool.addTxs [tx]
                blk <- Node.mineBlock
                replicateM_ 5 Node.mineBlock
                pure $ blk
            response <- Client.run (Client.getTransaction txHash)
            let expected = API.TxLookupResponse
                    { txHash = Crypto.hash tx
                    , txBlockHash = Just (blockHash blk)
                    , txConfirmations = 6
                    , txOutput = Just (Right [])
                    , txPayload = tx
                    }
            response @?= API.Ok expected
        ]
    ]

arbitraryHash :: IsCrypto c => Gen (Crypto.Hashed c a)
arbitraryHash = Crypto.toHashed . Crypto.fromHashed . Crypto.hash <$> (arbitrary :: Gen ByteString)

arbitraryIdent :: Gen Text
arbitraryIdent = T.pack <$> listOf1 (elements ['a'..'z'])
