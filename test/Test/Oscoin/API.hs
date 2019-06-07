-- | Tests behavior of the Node through its API using a test
-- implementation of 'MonadClient'.
module Test.Oscoin.API
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import           Oscoin.Data.Tx (txPubKey)

import qualified Oscoin.Test.API.HTTP.TestClient as Client
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitraryKeyPair)
import           Oscoin.Test.HTTP.Helpers
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck


tests :: forall c. Dict (IsCrypto c) -> TestTree
tests Dict = testGroup "Test.Oscoin.API"
    [ testGroup "getState"
        [ testProperty "existing value" $ monadicIO $ do
            key <- pick arbitrary
            value <- pick arbitrary
            runSessionWithState' @c [(key, value)] $ do
                result <- Client.getState Client.session key
                result @?= Just value

        , testProperty "non-existing value" $ monadicIO $ do
            key <- pick arbitrary
            runSessionWithState' @c [] $ do
              result <- Client.getState Client.session key
              result @?= Nothing
        ]

    , testGroup "submitTransaction" $
        [ testProperty "invalid transaction" $ monadicIO $ do
            (_, validTx) <- genDummyTx @c
            otherPubKey <- pick (fst <$> arbitraryKeyPair)
            let invalidTx = validTx { txPubKey = otherPubKey }
            runEmptySession @c $ do
                response <- Client.submitTransaction Client.session invalidTx
                response @?= API.Err "Invalid transaction"
        ]
    ]
