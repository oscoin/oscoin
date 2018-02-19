module Main (main) where

import           Oscoin.Prelude
import qualified Oscoin.Tests as Oscoin
import qualified Crypto.Tests as Crypto
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "All" [Oscoin.tests, Crypto.tests]
