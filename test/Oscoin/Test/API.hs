-- | Tests behavior of the Node through its API using a test
-- implementation of 'MonadClient'.
module Oscoin.Test.API
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import           Oscoin.Crypto.Blockchain.Block (blockHash)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool
import qualified Radicle.Extended as Rad

import qualified Data.Text as T

import qualified Oscoin.Test.API.HTTP.TestClient as Client
import           Oscoin.Test.HTTP.Helpers
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck


tests :: [TestTree]
tests =
    [ testGroup "getState"
        [ testCase "existing value" $ do
            path <- generate $ listOf $ arbitraryRadicleIdent
            -- TODO Generate arbitrary Radicle values once #258 is fixed
            let radValue = Rad.String "hooray!"
            let bindings = [(T.intercalate "/" path, radValue)]
            runSessionBindings bindings $ do
                result <- Client.run (Client.getState path)
                result @?= API.Ok radValue

        , testCase "non-existing value" $ runEmptySession $ do
            path <- liftIO $ generate $ listOf $ arbitraryRadicleIdent
            result <- Client.run (Client.getState path)
            result @?= API.Err "Value not found"

        , testCase "existing reference" $ do
            refName <- generate $ arbitraryRadicleIdent
            -- TODO Generate arbitrary Radicle values once #258 is fixed
            let radValue = Rad.String "hooray!"
            let env = initRadicleEnv []
                      & addRadicleRef refName radValue
            runSessionEnv env $ do
                result <- Client.run (Client.getState [refName])
                result @?= API.Ok radValue
        ]
    , testGroup "getTransaction" $

        [ testCase "missing transaction" $ runEmptySession $ do
            txHash <- liftIO $ generate arbitraryHash
            response <- Client.run (Client.getTransaction txHash)
            response @?= API.Err "Transaction not found"

        , testCase "confirmed transaction" $ runEmptySession $ do
            (txHash, tx) <- createValidTx (Rad.String "jo")
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

        , testCase "confirmed transaction" $ runEmptySession $ do
            let radValue = Rad.String "jo"
            (txHash, tx) <- createValidTx radValue
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
                    , txOutput = Just (Right radValue)
                    , txPayload = tx
                    }
            response @?= API.Ok expected
        ]
    ]

arbitraryHash :: Gen (Crypto.Hashed a)
arbitraryHash = Crypto.toHashed . Crypto.fromHashed . Crypto.hash <$> (arbitrary :: Gen ByteString)

arbitraryRadicleIdent :: Gen Text
arbitraryRadicleIdent = T.pack <$> listOf1 (elements ['a'..'z'])
