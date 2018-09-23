module Oscoin.Test.Helpers where

import qualified Data.Text as T
import           Oscoin.Prelude

import           Test.Tasty.HUnit

assertNoError :: Either Text a -> Assertion
assertNoError (Left err) = assertFailure (T.unpack $ "Error: " <> err)
assertNoError (Right _)  = pass

assertError :: Either Text a -> Assertion
assertError (Right _) = assertFailure "Expected error, but got none"
assertError (Left _)  = pass
