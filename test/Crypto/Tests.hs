module Crypto.Tests where

import           Prelude

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Crypto.Data.Auth.Tree as Tree

tests :: TestTree
tests = testGroup "Crypto"
    [ testCase       "Crypto.Data.Auth.Tree"          testAuthTree ]

testAuthTree :: Assertion
testAuthTree = do
    let tree = Tree.empty
    Tree.lookup "key" (Tree.insert "key" "val" tree) @?= Just "val"

