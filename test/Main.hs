module Main (main) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto
import           Oscoin.Crypto.Hash.Mock ()
import           Oscoin.Crypto.PubKey.Mock ()
import           Oscoin.Environment (Environment(Testing))

import qualified Crypto.Test.Hash.Multi as Multihash
import qualified Integration.Tests as Integration
import           Oscoin.Test.Crypto
import qualified Oscoin.Tests as Oscoin
import qualified Test.Control.Concurrent.RateLimit
import qualified Test.Data.Conduit.Serialise
import qualified Test.Data.Sequence.Circular
import qualified Test.Oscoin.Crypto.Hash
import qualified Test.Oscoin.Crypto.PubKey
import           Test.Tasty
import           Test.Tasty.Ingredients.FailFast

type CryptoUnderTest = IsCrypto MockCrypto

main :: IO ()
main = do
    let config = Consensus.getConfig Testing
    let crypto = Dict @CryptoUnderTest
    defaultMainWithIngredients (map failFast defaultIngredients) $ testGroup "All"
        [ Oscoin.tests crypto config
        , Multihash.tests
        , Integration.tests
        , Test.Control.Concurrent.RateLimit.tests
        , Test.Data.Sequence.Circular.tests
        , Test.Data.Conduit.Serialise.tests
        , Test.Oscoin.Crypto.Hash.tests crypto
        , Test.Oscoin.Crypto.PubKey.tests crypto
        ]
