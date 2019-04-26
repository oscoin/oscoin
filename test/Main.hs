{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Main (main) where

import           Oscoin.Prelude

import           Oscoin.Configuration (Environment(Development))
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
import qualified Test.Oscoin.Configuration
import qualified Test.Oscoin.Crypto.Blockchain.Eval
import qualified Test.Oscoin.Crypto.Blockchain.GeneratorsTest
import qualified Test.Oscoin.Crypto.Hash
import qualified Test.Oscoin.Crypto.PubKey
import qualified Test.Oscoin.Node.Mempool
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

withCrypto :: CryptoUnderTest -> (forall c. Dict (IsCrypto c) -> a) -> a
withCrypto RealWorldCryptoTest f = f (Dict :: Dict (IsCrypto Crypto))
withCrypto MockCryptoTest      f = f (Dict :: Dict (IsCrypto MockCrypto))

main :: IO ()
main = do
    let config = Consensus.configForEnvironment Development
    let ingredients = map (failFast . selectCrypto) defaultIngredients

    defaultMainWithIngredients ingredients $
        askOption $ \selectedCrypto ->
            withCrypto selectedCrypto $ \crypto@Dict -> testGroup "All"
                [ Oscoin.tests crypto config
                , Integration.tests
                , Test.Control.Concurrent.RateLimit.tests
                , Test.Data.Conduit.Serialise.tests
                , Test.Data.Sequence.Circular.tests
                , Test.Oscoin.API.tests crypto
                , Test.Oscoin.Configuration.tests
                , Test.Oscoin.Crypto.Blockchain.Eval.tests crypto
                , Test.Oscoin.Crypto.Blockchain.GeneratorsTest.tests crypto
                , Test.Oscoin.Crypto.Hash.tests crypto
                , Test.Oscoin.Crypto.PubKey.tests crypto
                , Test.Oscoin.Node.Mempool.tests crypto
                , Test.Oscoin.Protocol.tests crypto
                , Test.Oscoin.Storage.Block.Orphanage.tests crypto
                , Test.Oscoin.Storage.Ledger.tests crypto
                ]
