module Main (main) where

import qualified Crypto.Tests as Crypto
import           Oscoin.Prelude
import qualified Oscoin.Tests as Oscoin
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "All" [Oscoin.tests, Crypto.tests]
