module Crypto.Tests where

import           Prelude

import           Data.List (sort)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Word
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Either (isRight)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Crypto.Data.Auth.Tree as Tree
import qualified Crypto.Data.Auth.Tree.Proof as Tree
import           Crypto.Data.Auth.Tree (Tree)
import           Crypto.Hash (SHA256)

type Key = Word8
type Val = Word8

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Tree.Tree k v) where
    arbitrary = do
        inserts <- arbitrary :: Gen [(k, v)]
        deletes <- map fst <$> sublistOf inserts
        pure (foldr ($) Tree.empty (updates inserts deletes))
      where
        updates :: [(k, v)] -> [k] -> [Tree k v -> Tree k v]
        updates inserts deletes = [Tree.delete k   |  k     <- deletes]
                               ++ [Tree.insert k v | (k, v) <- inserts]

tests :: TestTree
tests = localOption (QuickCheckTests 200) $ testGroup "Crypto"
    [ testProperty    "Insert/Lookup"          propInsertLookup
    , testProperty    "Insert/Elem"            propInsertElem
    , testProperty    "Delete/Elem"            propDelete
    , testProperty    "Invariant"              propInvariant
    , testProperty    "Sorted"                 propSorted
    , testProperty    "List From/To"           propList
    , testProperty    "Inner nodes"            propInnerNodes
    , testProperty    "AVL-balanced"           propBalanced
    , testProperty    "Proofs"                 propProofVerify
    ]

-- | Updates must preserve ordering.
propInvariant :: Tree Key Val -> Bool
propInvariant tree =
    propSorted tree && propBalanced tree && propInnerNodes tree

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

propInnerNodes :: Tree Key Val -> Bool
propInnerNodes tree =
    all (propInnerNode >> propBalanced) (Tree.flatten tree)

propInnerNode :: Tree Key Val -> Bool
propInnerNode (Tree.Node k _ r) =
    k == Tree.leftmost r
propInnerNode _ = True

-- | Valid proofs of key existence verify positively.
propProofVerify :: Tree Key Val -> Key -> Val -> Bool
propProofVerify tree' k v | tree <- Tree.insert k v tree'
                          , root <- Tree.merkleHash tree =
    case Tree.lookup' k tree :: (Maybe Val, Tree.Proof SHA256 Key Val) of
        (Just v, proof) -> isRight (Tree.verify proof root k v)
        _               -> False
