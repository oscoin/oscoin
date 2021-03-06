{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Main (main) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto
import           Oscoin.Crypto.Hash.Mock ()
import           Oscoin.Crypto.PubKey.Mock ()

import qualified Integration.Tests as Integration
import           Oscoin.Test.Crypto
import qualified Oscoin.Tests as Oscoin
import qualified Test.Control.Concurrent.RateLimit
import qualified Test.Data.Conduit.Serialise
import qualified Test.Data.Sequence.Circular
import qualified Test.Oscoin.API
import qualified Test.Oscoin.API.HTTP
import qualified Test.Oscoin.Configuration
import qualified Test.Oscoin.Crypto.Blockchain.Eval
import qualified Test.Oscoin.Crypto.Blockchain.GeneratorsTest
import qualified Test.Oscoin.Crypto.Blockchain.Genesis
import qualified Test.Oscoin.Crypto.Hash
import qualified Test.Oscoin.Crypto.PubKey
import qualified Test.Oscoin.Crypto.PubKey.FileStore
import qualified Test.Oscoin.Data.OscoinTx
import qualified Test.Oscoin.Node.Mempool
import qualified Test.Oscoin.Node.Options
import qualified Test.Oscoin.Protocol
import qualified Test.Oscoin.Storage.Block.Orphanage
import qualified Test.Oscoin.Storage.Ledger
import           Test.Tasty
import           Test.Tasty.Ingredients as Tasty
import           Test.Tasty.Ingredients.FailFast
import           Test.Tasty.Options as Tasty

data CryptoUnderTest =
      RealWorldCryptoTest
    | MockCryptoTest
    deriving (Show, Eq, Typeable)

cryptoName :: CryptoUnderTest -> String
cryptoName RealWorldCryptoTest = "RealWorld Crypto"
cryptoName MockCryptoTest      = "Mock Crypto"

readCryptoUnderTest :: String -> Maybe CryptoUnderTest
readCryptoUnderTest "realworld" = Just RealWorldCryptoTest
readCryptoUnderTest "mock"      = Just MockCryptoTest
readCryptoUnderTest _           = Nothing

instance Tasty.IsOption CryptoUnderTest where
  defaultValue = MockCryptoTest
  parseValue = readCryptoUnderTest
  optionName = pure "crypto"
  optionHelp = pure "The crypto implementation to use (either 'realworld' or 'mock')"

-- | Extends a TestReporter with a '--crypto' option, so that 'Tasty' won't
-- choke when '--crypto' is passed.
selectCrypto :: Tasty.Ingredient -> Tasty.Ingredient
selectCrypto (TestReporter opts f) = TestReporter (ffOpt:opts) f
  where ffOpt = Tasty.Option (Proxy :: Proxy CryptoUnderTest)
selectCrypto i = i -- not applicable

realCrypto :: Dict (IsCrypto Crypto)
realCrypto = Dict

mockCrypto :: Dict (IsCrypto MockCrypto)
mockCrypto = Dict

withCrypto :: CryptoUnderTest -> (forall c. Dict (IsCrypto c) -> a) -> a
withCrypto RealWorldCryptoTest f = f realCrypto
withCrypto MockCryptoTest      f = f mockCrypto

main :: IO ()
main = do
    let config = Consensus.testConfig
    let ingredients = map (failFast . selectCrypto) defaultIngredients

    defaultMainWithIngredients ingredients $
        askOption $ \selectedCrypto ->
            -- Run tests with the selected crypto.
            --
            -- Some tests are run with both the mock and real crypto, since
            -- they are testing the actual crypto implementations.
            withCrypto selectedCrypto $ \crypto@Dict -> testGroup "All"
                [ testGroup (cryptoName selectedCrypto)
                    [ Oscoin.tests crypto config
                    , Integration.tests
                    , Test.Control.Concurrent.RateLimit.tests
                    , Test.Data.Conduit.Serialise.tests
                    , Test.Data.Sequence.Circular.tests
                    , Test.Oscoin.API.tests crypto
                    , Test.Oscoin.API.HTTP.tests crypto
                    , Test.Oscoin.Configuration.tests
                    , Test.Oscoin.Crypto.Blockchain.Eval.tests crypto
                    , Test.Oscoin.Crypto.Blockchain.GeneratorsTest.tests crypto
                    , Test.Oscoin.Crypto.PubKey.FileStore.tests
                    , Test.Oscoin.Data.OscoinTx.tests crypto
                    , Test.Oscoin.Node.Mempool.tests crypto
                    , Test.Oscoin.Node.Options.tests crypto
                    , Test.Oscoin.Protocol.tests crypto
                    , Test.Oscoin.Storage.Block.Orphanage.tests crypto
                    , Test.Oscoin.Storage.Ledger.tests crypto
                    , Test.Oscoin.Crypto.Blockchain.Genesis.tests crypto
                    ]
                , testGroup (cryptoName MockCryptoTest)
                    [ Test.Oscoin.Crypto.Hash.tests mockCrypto
                    , Test.Oscoin.Crypto.PubKey.tests mockCrypto
                    ]
                , testGroup (cryptoName RealWorldCryptoTest)
                    [ Test.Oscoin.Crypto.Hash.tests realCrypto
                    , Test.Oscoin.Crypto.PubKey.tests realCrypto
                   ]
                ]
