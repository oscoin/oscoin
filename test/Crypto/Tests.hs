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
tests = localOption (QuickCheckTests 1000) $ testGroup "Crypto"
    [ testProperty    "Insert/Lookup"          propInsertLookup
    , testProperty    "Insert/Elem"            propInsertElem
    , testProperty    "Delete/Elem"            propDelete
    , testProperty    "Invariant"              propInvariant
    , testProperty    "Sorted"                 propSorted
    , testProperty    "List From/To"           propList
    , testProperty    "AVL-balanced"           propBalanced
    ]

-- | Updates must preserve ordering.
propInvariant :: Tree Key Val -> Gen Bool
propInvariant tree = do
    inserts <- arbitrary :: Gen [(Key, Val)]
    deletes <- map fst <$> sublistOf inserts
    updated <- pure (foldr ($) tree (updates inserts deletes))
    pure $ propSorted updated && propBalanced updated
  where
    updates :: [(Key, Val)] -> [Key] -> [Tree Key Val -> Tree Key Val]
    updates inserts deletes = [Tree.insert k v | (k, v) <- inserts]
                           ++ [Tree.delete k   |  k     <- deletes]

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

-- | Deleting all the keys in a tree should result in an empty tree.
propDelete :: Tree.Tree Key Val -> Property
propDelete tree =
    not (Tree.null tree) ==>
        Tree.null $ foldr Tree.delete tree (Tree.keys tree)

-- | The balance factor of an AVL tree should always be in the set {-1, 0, 1}.
propBalanced :: Tree Key Val -> Bool
propBalanced tree =
    fromEnum (Tree.balance tree) `elem` [-1, 0, 1]
