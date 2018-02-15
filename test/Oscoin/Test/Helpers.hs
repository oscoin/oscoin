module Oscoin.Test.Helpers where

import           Oscoin.Prelude
import qualified Data.Text as T

import           Test.Tasty.HUnit

assertNoError :: Either Error a -> Assertion
assertNoError (Left err) = assertFailure (T.unpack $ "Error: " <> fromError err)
assertNoError (Right _)  = pass
