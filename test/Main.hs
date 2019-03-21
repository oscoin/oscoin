module Main (main) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto
import           Oscoin.Crypto.Hash.Mock ()
import           Oscoin.Crypto.PubKey.Mock ()
import           Oscoin.Environment (Environment(Testing))

import qualified Control.Concurrent.Tests as Concurrent
import qualified Crypto.Test.Hash.Multi as Multihash
import qualified Data.Conduit.Tests as Conduit
import qualified Data.Sequence.Circular.Tests as Circular
import qualified Integration.Tests as Integration
import           Oscoin.Test.Crypto
import qualified Oscoin.Tests as Oscoin
import           Test.Tasty
import           Test.Tasty.Ingredients.FailFast

type CryptoUnderTest = IsCrypto MockCrypto

main :: IO ()
main = do
    let config = Consensus.getConfig Testing
    defaultMainWithIngredients (map failFast defaultIngredients) $ testGroup "All"
        [ Oscoin.tests (Dict @CryptoUnderTest) config
        , Multihash.tests
        , Concurrent.tests
        , Conduit.tests
        , Integration.tests
        , Circular.tests
        ]
