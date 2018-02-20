module Crypto.Tests where

import           Prelude

import           Data.List (sort)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.List.NonEmpty (NonEmpty, toList)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Crypto.Data.Auth.Tree as Tree
import           Crypto.Data.Auth.Tree (Tree)

type Key = Int
type Val = Int

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Tree.Tree k v) where
    arbitrary = do
        kvs <- arbitrary
        pure $ Tree.fromList kvs

tests :: TestTree
tests = testGroup "Crypto"
    [ testProperty    "Insert/Lookup"          propInsertLookup
    , testProperty    "Insert/Elem"            propInsertElem
    , testProperty    "Invariant"              propInvariant
    , testProperty    "Sorted"                 propSorted
    , testProperty    "List From/To"           propList
    ]

-- | Updates must preserve ordering.
propInvariant :: Tree Key Val -> NonEmpty (Key, Val) -> Bool
propInvariant tree (toList -> inserts) =
    propSorted (foldr ($) tree updates)
  where
    updates :: [Tree Key Val -> Tree Key Val]
    updates = [Tree.insert k v | (k, v) <- inserts]
           ++ [Tree.delete k   | (k, _) <- inserts]

propSorted :: Tree Key Val -> Bool
propSorted (Tree.toList -> kvs) =
    sort kvs == kvs

propList :: Set Key -> NonEmpty Val -> Bool
propList (Set.toList -> ks) (toList -> vs) =
    (Tree.toList . Tree.fromList) kvs == kvs
  where
    kvs = zip ks (cycle vs)

propInsertLookup :: Tree.Tree Key Val -> Key -> Val -> Property
propInsertLookup tree k v =
    not (Tree.elem k tree) ==>
        Tree.lookup k (Tree.insert k v tree) == Just v

propInsertElem :: Tree.Tree Key Val -> Key -> Val -> Property
propInsertElem tree k v =
    not (Tree.elem k tree) ==>
        Tree.elem k (Tree.insert k v tree)
