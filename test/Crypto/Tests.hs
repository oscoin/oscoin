module Crypto.Tests where

import           Prelude

import           Data.List (sort)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Word
import           Data.List.NonEmpty (NonEmpty, toList)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Crypto.Data.Auth.Tree as Tree
import           Crypto.Data.Auth.Tree (Tree)

type Key = Word8
type Val = Word8

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
    , testProperty    "AVL-balanced"           propBalanceFactor
    ]

-- | Updates must preserve ordering.
propInvariant :: Tree Key Val -> NonEmpty (Key, Val) -> Bool
propInvariant tree (toList -> inserts) =
    propSorted (foldr ($) tree updates)
  where
    updates :: [Tree Key Val -> Tree Key Val]
    updates = [Tree.insert k v | (k, v) <- inserts]
           ++ [Tree.delete k   | (k, _) <- inserts]

-- | The tree is always sorted.
propSorted :: Tree Key Val -> Bool
propSorted (Tree.toList -> kvs) =
    sort kvs == kvs

-- | Converting to/from a list should preverse the data.
--
-- Note that unique keys are required for this invariant to hold, as duplicate
-- keys will be overwritten.
propList :: Set Key -> NonEmpty Val -> Bool
propList (Set.toList -> ks) (cycle . toList -> vs) =
    (Tree.toList . Tree.fromList) kvs == kvs
  where
    kvs = zip ks vs

-- | Keys inserted should be available for lookup.
propInsertLookup :: Tree.Tree Key Val -> Key -> Val -> Bool
propInsertLookup tree k v =
    Tree.lookup k (Tree.insert k v tree) == Just v

-- | Keys inserted should be elements of the tree.
propInsertElem :: Tree.Tree Key Val -> Key -> Val -> Property
propInsertElem tree k v =
    not (Tree.elem k tree) ==>
        Tree.elem k (Tree.insert k v tree)

-- | The balance factor of an AVL tree should always be in the set {-1, 0, 1}.
propBalanceFactor :: Tree Key Val -> Bool
propBalanceFactor tree =
    fromEnum (Tree.balance tree) `elem` [-1, 0, 1]
