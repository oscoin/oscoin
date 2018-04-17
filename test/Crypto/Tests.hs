module Crypto.Tests where

import           Prelude hiding (length)
import qualified Prelude

import           Data.List (sort)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Word
import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Either (isLeft, isRight)
import           Control.Monad.Fail
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Test.QuickCheck.Instances ()

import qualified Crypto.Data.Auth.Tree as Tree
import qualified Crypto.Data.Auth.Tree.Proof as Tree
import           Crypto.Data.Auth.Tree (Tree)
import           Crypto.Hash (SHA256)

type Key = Word8
type Val = Word8

instance MonadFail Gen where
    fail err = error err

instance ByteArrayAccess Word8 where
    length = const 1
    withByteArray b = withByteArray (BS.singleton b)

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
tests = localOption (QuickCheckTests 100) $ testGroup "Crypto"
    [ testProperty    "Insert/Lookup"          propInsertLookup
    , testProperty    "Insert/Member"          propInsertMember
    , testProperty    "Delete/Member"          propDelete
    , testProperty    "Invariant"              propInvariant
    , testProperty    "Sorted"                 propSorted
    , testProperty    "List From/To"           propList
    , testProperty    "Inner nodes"            propInnerNodes
    , testProperty    "AVL-balanced"           propBalanced
    , testProperty    "Proofs"                 propProofVerify
    , testProperty    "More proofs"            propProofNotVerify
    , testProperty    "Absence proofs"         propProofAbsenceNotVerify
    , testProperty    "Pred/Succ"              propPredSucc
    , testProperty    "First"                  propFirst
    , testProperty    "Last"                   propLast
    , testProperty    "Size"                   propSize
    , testProperty    "Succ"                   propSucc
    , testCase        "Union"                  testUnion
    , testCase        "Empty tree Proof"       testEmptyTreeProof
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

-- | Keys inserted should be members of the tree.
propInsertMember :: Tree.Tree Key Val -> Key -> Val -> Property
propInsertMember tree k v =
    not (Tree.member k tree) ==>
        Tree.member k (Tree.insert k v tree)

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
    k == fst (Tree.findMin r)
propInnerNode _ = True

testUnion :: Assertion
testUnion =
    Tree.union t1 t2 @?= Tree.fromList [('a', 1), ('b', 1), ('d', 2)]
  where
    t1, t2 :: Tree Char Int
    t1 = Tree.fromList [('a', 1), ('b', 1)]
    t2 = Tree.fromList [('a', 2), ('d', 2)]

-- | Valid proofs of key existence verify positively.
propProofVerify :: Tree Key Val -> Key -> Val -> Bool
propProofVerify tree' k v | tree <- Tree.insert k v tree'
                          , root <- Tree.merkleHash tree =
    case Tree.lookupProof @SHA256 k tree of
        (Just v, proof) -> isRight (Tree.verify proof root k (Just v))
        _               -> False

-- | Valid proofs of key existence against the wrong key verify negatively.
propProofNotVerify :: Tree Key Val -> Key -> Val -> Gen Bool
propProofNotVerify tree k v = do
    ks <- arbitrary :: Gen [Key]

    -- Add a set of additional keys to the tree with the same value `v`.
    let t = Tree.union tree (Tree.fromList $ (k, v) : [(k', v) | k' <- ks])
    let root = Tree.merkleHash t

    pure $ case Tree.lookupProof @SHA256 k t of
        (Just _, proof) ->
            all isLeft [ Tree.verify proof root k' (Just v)
                       | (k', v) <- Tree.toList t
                       , k' /= k ]
        _ ->
            False

data TreeTest k v = TreeTest
    { tTree           :: Tree k v
    , tExistsKeyVal   :: (k, v)
    , tAbsentKey      :: k
    , tKeyExistsProof :: Tree.Proof SHA256 k v
    , tKeyAbsentProof :: Tree.Proof SHA256 k v
    } deriving (Eq)

instance (Show k, Show v) => Show (TreeTest k v) where
    show TreeTest{..} =
        unlines [ show tTree
        , "exists=" ++ show tExistsKeyVal
        , "absent=" ++ show tAbsentKey
        , ""
        , show tKeyExistsProof
        , show tKeyAbsentProof
        ]

-- | Arbitrary instance for 'TreeTest'.
instance ( ByteArrayAccess k
         , ByteArrayAccess v
         , Arbitrary k
         , Arbitrary v
         , Ord k
         ) => Arbitrary (TreeTest k v) where
    arbitrary = do
        -- Create a non-empty tree.
        tree <- arbitrary `suchThat` (\t -> not (Tree.null t)) :: Gen (Tree k v)
        -- Select an arbitrary key from the tree.
        present <- elements (Tree.keys tree)
        -- Create an arbitrary key which isn't in the tree.
        absent <- arbitrary `suchThat` (\k -> not (Tree.member k tree)) :: Gen k
        -- Create a proof of existence of the existing key.
        (Just v, existsProof) <- pure $ Tree.lookupProof present tree
        -- Create a proof of absence of the absent key.
        (Nothing, absentProof) <- pure $ Tree.lookupProof absent tree
        -- Return a TreeTest object with all of the above.
        pure $ TreeTest tree (present, v) absent existsProof absentProof

propProofAbsenceNotVerify :: TreeTest Key Val -> Gen Bool
propProofAbsenceNotVerify TreeTest{..} = pure $
    and [ isRight $ Tree.verify tKeyAbsentProof root tAbsentKey          Nothing
        , isLeft  $ Tree.verify tKeyAbsentProof root (fst tExistsKeyVal) Nothing
        , isRight $ Tree.verify tKeyExistsProof root (fst tExistsKeyVal) (Just (snd tExistsKeyVal))
        , isLeft  $ Tree.verify tKeyExistsProof root tAbsentKey          (Just (snd tExistsKeyVal))
        ]
  where
    root = Tree.merkleHash tTree

testEmptyTreeProof :: Assertion
testEmptyTreeProof = do
    case Tree.lookupProof @SHA256 @Key @Val 1 Tree.empty of
        (Nothing, proof) -> Tree.verify proof Tree.emptyHash 1 Nothing @?= Right ()
        _                -> assertFailure "Key was found in empty tree"

-- | > The successor of the predecessor of `k` is `k`.
propPredSucc :: Tree Key Val -> Property
propPredSucc t =
    preconditions ==>
        all predicate (Tree.keys t)
  where
    preconditions :: Bool
    preconditions =
        and [ not $ Tree.null t
            , not $ (fst <$> Tree.lookupMax t)  == Just maxBound
            , not $ (fst <$> Tree.lookupMin t) == Just minBound
            ]
    predicate :: Key -> Bool
    predicate k
        -- `k` is the first key in the tree. Check that the key that would
        -- preceed it has `k` as its successor.
        | Nothing      <- Tree.pred k t
        , Just (k', _) <- Tree.succ (pred k) t = k' == k

        -- `k` is the last key in the tree. Check that the key that would
        -- succeed it has `k` as its predecessor
        | Nothing      <- Tree.succ k t
        , Just (k', _) <- Tree.pred (succ k) t = k' == k

        -- `k` is somewhere in the middle, check that succ . pred == id.
        | Just (p, _) <- Tree.pred k t
        , Just (s, _) <- Tree.succ p t = s == k

        | otherwise = False

propSucc :: Tree Key Val -> Key -> Property
propSucc tree k =
    preconditions ==>
        case Tree.succ k tree of
            Just (k', _) -> k' > k && Tree.member k' tree
            _            -> False
  where
    preconditions =
        case Tree.bounds tree of
            Just (l, r) -> l <= k && k < r
            Nothing     -> False

-- | The first element of a tree has no predecessor.
propFirst :: Tree Key Val -> Property
propFirst tree =
    not (Tree.null tree) ==>
        case Tree.lookupMin tree of
            Just (k, _) -> Tree.pred k tree == Nothing
            _            -> False

-- | The last element of a tree has no successor.
propLast :: Tree Key Val -> Property
propLast tree =
    not (Tree.null tree) ==>
        case Tree.lookupMax tree of
            Just (k, _) -> Tree.succ k tree == Nothing
            _            -> False

propSize :: Tree Key Val -> Bool
propSize tree =
    Prelude.length (Tree.toList tree) == Tree.size tree
