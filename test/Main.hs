module Main (main) where

import           Oscoin.Prelude

import qualified Crypto.Test.Hash.Multi as Multihash
import qualified Crypto.Tests as Crypto
import qualified Oscoin.Tests as Oscoin
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "All" [Oscoin.tests, Crypto.tests, Multihash.tests]
