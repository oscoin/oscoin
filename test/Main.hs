module Main (main) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Environment (Environment(Testing))

import qualified Control.Concurrent.Tests as Concurrent
import qualified Crypto.Test.Hash.Multi as Multihash
import qualified Data.Conduit.Tests as Conduit
import qualified Integration.Tests as Integration
import qualified Oscoin.Tests as Oscoin
import           Test.Tasty
import           Test.Tasty.Ingredients.FailFast

main :: IO ()
main = do
    config <- Consensus.getConfig Testing
    defaultMainWithIngredients (map failFast defaultIngredients) $ testGroup "All"
        [ Oscoin.tests config
        , Multihash.tests
        , Concurrent.tests
        , Conduit.tests
        , Integration.tests
        ]
