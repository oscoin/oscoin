module Main (main) where

import           Oscoin.Prelude

import qualified Control.Concurrent.Tests as Concurrent
import qualified Crypto.Test.Hash.Multi as Multihash
import qualified Crypto.Tests as Crypto
import qualified Data.Conduit.Tests as Conduit
import qualified Oscoin.Tests as Oscoin
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All"
    [ Oscoin.tests
    , Crypto.tests
    , Multihash.tests
    , Concurrent.tests
    , Conduit.tests
    ]
