module Crypto.Data.Auth.Tree
    ( Tree(..)
    , empty
    , member
    , insert
    , lookup
    , lookup'
    , delete
    , null
    , toList
    , fromList
    , map
    , mapWithKey
    , union
    , values
    , pred
    , succ
    , first
    , last
    , keys
    , flatten
    , leftmost
    , verify
    , merkleHash
    , emptyHash

    -- * Utility types and functions
    , Height
    , height

    , Balance
    , balance
    ) where

import           Crypto.Data.Auth.Tree.Proof
import           Crypto.Hash (HashAlgorithm, Digest, hash, digestFromByteString, hashDigestSize)
import qualified Data.Binary as Binary
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.ByteArray (zero, convert)
import           Data.Maybe (fromJust)

import qualified Prelude as Prelude
import           Prelude hiding (lookup, null, elem, foldr, map, traverse, pred, succ, last)
import qualified Data.List as List
import           GHC.Generics

-- | An AVL+ tree.
--
-- * Note: The key of an inner node is always the left-most key of its right
--   sub-tree.
-- * Note: AVL+ trees only store values at the leaves.
--
data Tree k v =
      Empty
    | Node k (Tree k v) (Tree k v)
    | Leaf k v
    deriving (Functor, Traversable, Foldable, Generic)

instance (Eq k, Eq v) => Eq (Tree k v) where
    (==) t1 t2 = toList t1 == toList t2

instance (Binary k, Binary v) => Binary (Tree k v)

merkleHash
    :: forall k v a. (Binary k, Binary v, HashAlgorithm a)
    => Tree k v
    -> Digest a
merkleHash Empty =
    emptyHash
merkleHash (Leaf k v) =
    hash (LBS.toStrict $ Binary.encode (k, v))
merkleHash (Node _ l r) =
    hashOfHashes [merkleHash l, merkleHash r]

emptyHash :: forall a. HashAlgorithm a => Digest a
emptyHash =
    fromJust $ digestFromByteString (zero n :: ByteString)
  where
    n = hashDigestSize (undefined :: a)

hashOfHashes :: HashAlgorithm a => [Digest a]  -> Digest a
hashOfHashes =
    hash . BS.concat . fmap convert

-- TODO: Use hashInit/hashUpdate for performance?
-- | Verify a proof.
verify
    :: (Binary k, Binary v, HashAlgorithm a)
    => Proof a k v
    -> Digest a
    -> k
    -> Maybe v
    -> Either String ()
verify (KeyExistsProof path) root k (Just v)
    | pathDigest path k v == root = Right ()
    | otherwise                   = Left "Root's don't match"
verify (KeyAbsentProof Nothing Nothing) root _ Nothing
    | root == emptyHash = Right ()
    | otherwise         = Left "Empty absence proof"
verify _ _ _ _ = undefined

pathDigest
    :: (Binary k, Binary v, HashAlgorithm a) => Path a () -> k -> v -> Digest a
pathDigest (Path elems _) k v =
    List.foldl' (flip digest) (merkleHash (Leaf k v)) elems
  where
    digest (L l) r = hashOfHashes [l, r]
    digest (R r) l = hashOfHashes [l, r]

-- | Create an empty tree.
empty :: Tree k v
empty = Empty

-- | /O(log n)/. Return 'True' if the key is an element of the tree.
member :: Ord k => k -> Tree k v -> Bool
member _ Empty = False
member k (Leaf k' _) = k == k'
member k (Node k' l r)
    | k < k'    = member k l
    | otherwise = member k r

-- | /O(log n)/. Insert a key and value into a tree.
insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert k v Empty = Leaf k v
insert k v (Node k' l r)
    | k < k'    = rebalance $ Node k' (insert k v l) r
    | otherwise = rebalance $ Node k' l (insert k v r)
insert k v (Leaf k' v')
    | k == k' = Leaf k' v
    | k >  k' = rebalance $ Node k (Leaf k' v') (Leaf k v)
    | k <  k' = rebalance $ Node k' (Leaf k v) (Leaf k' v')
insert _ _ _ =
    undefined

-- | /O(log n)/. Lookup a key from a tree.
lookup :: Ord k => k -> Tree k v -> Maybe v
lookup k (Leaf k' v) | k == k' = Just v
lookup k (Node k' l r)
    | k < k'    = lookup k l
    | otherwise = lookup k r
lookup _ _ = Nothing

lookup'
    :: (Binary k, Binary v, HashAlgorithm d, Ord k)
    => k
    -> Tree k v
    -> (Maybe v, Proof d k v)
lookup' k tree =
    f k tree []
  where
    f k (Leaf k' v) path
        | k == k'   = (Just v, KeyExistsProof (Path path ()))
        | otherwise = (Nothing, keyAbsentProof k tree)
    f k (Node k' l r) path
        | k < k'    = f k l (R (merkleHash r) : path)
        | otherwise = f k r (L (merkleHash l) : path)
    f _ Empty _ =
        (Nothing, KeyAbsentProof Nothing Nothing)

keyAbsentProof :: (HashAlgorithm a, Ord k) => k -> Tree k v -> Proof a k v
keyAbsentProof k tree = undefined

pred :: Ord k => k -> Tree k v -> Maybe (k, v)
pred k (Node k' l r)
    | k == k'   = Just (rightmost l)
    | k <  k'   = pred k l
    | otherwise = pred k r
pred k (Leaf k' v) | k' < k = Just (k', v)
pred _ _ = Nothing

succ :: Ord k => k -> Tree k v -> Maybe (k, v)
succ k (Node k' l r)
    | k <  k'   = if   fst (rightmost l) == k
                  then Just (leftmost r)
                  else succ k l
    | otherwise = succ k r
succ k (Leaf k' v) | k' > k = Just (k', v)
succ _ _ = Nothing

first :: Tree k v -> Maybe (k, v)
first Empty = Nothing
first tree  = Just (leftmost tree)

last :: Tree k v -> Maybe (k, v)
last Empty = Nothing
last tree  = Just (rightmost tree)

-- | /O(log n)/. Delete a key from a tree.
delete :: Ord k => k -> Tree k v -> Tree k v
delete _ Empty = Empty
delete k leaf@(Leaf k' _)
    | k == k'   = Empty
    | otherwise = leaf
delete k (Node k' l r)
    | k < k'    = rebalance . collapse $ Node k' (delete k l) r
    | otherwise = rebalance . collapse $ let r' = delete k r in Node (fst $ leftmost r') l r'
  where
    collapse (Node _ l Empty) = l
    collapse (Node _ Empty r) = r
    collapse tree             = tree

-- | Get the left most key of a tree.
leftmost :: Tree k v -> (k, v)
leftmost (Node _ l _) = leftmost l
leftmost (Leaf k v)   = (k, v)
leftmost Empty        = undefined

-- | Get the right most key of a tree.
rightmost :: Tree k v -> (k, v)
rightmost (Node _ _ r) = rightmost r
rightmost (Leaf k v)   = (k, v)
rightmost Empty        = undefined

-- | Convert a tree into a list of @(k, v)@ pairs.
toList :: Tree k v -> [(k, v)]
toList Empty = []
toList (Leaf k v) = [(k, v)]
toList (Node _ l r) = toList l ++ toList r

fromList :: Ord k => [(k, v)] -> Tree k v
fromList kvs = List.foldl' (\tree (k, v) -> insert k v tree) Empty kvs

-- | Return 'True' if a tree is empty.
null :: Tree k v -> Bool
null Empty = True
null _     = False

-- | Map a function with a key onto the values of a tree.
mapWithKey :: Ord k => (k -> a -> b) -> Tree k a -> Tree k b
mapWithKey f tree = fromList $ List.map (\(k, v) -> (k, f k v)) (toList tree)

-- | Map a function onto the values of a tree.
map :: (a -> b) -> Tree k a -> Tree k b
map = fmap

-- | Left-biased union of two trees.
union :: Ord k => Tree k v -> Tree k v -> Tree k v
union t1 t2 =
    List.foldl' (\tree (k, v) -> insert k v tree) t2 (toList t1)

-- | Return the values of a tree in-order.
values :: Tree k v -> [v]
values tree = List.map snd (toList tree)

-- | Return the keys of a tree in-order.
keys :: Tree k v -> [k]
keys tree = List.map fst (toList tree)

-- | Flatten a tree to a list of nodes in a depth-first manner.
flatten :: Tree k v -> [Tree k v]
flatten Empty             = []
flatten leaf@(Leaf _ _)   = [leaf]
flatten node@(Node _ l r) = node : flatten l ++ flatten r

-- Utility --------------------------------------------------------------------

newtype Height = Height { fromHeight :: Int }
    deriving (Num, Eq, Ord)

instance Show Height where
    show = show . fromHeight

height :: Tree k v -> Height
height Empty        = 0
height (Leaf _ _)   = 0
height (Node _ l r) = 1 + max (height l) (height r)

data Balance =
      LeftHeavy  Int
    | RightHeavy Int
    | Balanced
    deriving (Eq, Show)

instance Enum Balance where
    toEnum n
        | n > 0     = RightHeavy n
        | n < 0     = LeftHeavy n
        | otherwise = Balanced

    fromEnum (LeftHeavy n)  = n
    fromEnum (RightHeavy n) = n
    fromEnum Balanced       = 0

balance :: Tree k v -> Balance
balance Empty        = Balanced
balance (Leaf _ _)   = Balanced
balance (Node _ l r)
    | b < 0     = LeftHeavy  b
    | b > 0     = RightHeavy b
    | otherwise = Balanced
  where
    b = fromHeight (height r - height l)

rebalance :: Tree k v -> Tree k v
rebalance tree@(Node _ l r) =
    case balance tree of
        LeftHeavy  n | n < -1 ->
            case balance l of
                RightHeavy _ -> rotLR tree
                _            -> rotR tree
        RightHeavy n | n > 1 ->
            case balance r of
                LeftHeavy _ -> rotRL tree
                _           -> rotL tree
        _ -> tree
rebalance tree = tree

rotL, rotR, rotLR, rotRL :: Tree k v -> Tree k v

-- | Simple left rotation.
--
--      n1             n2
--     / \            /  \
--    a   n2    →    n1   c
--       /  \       /  \
--      b    c     a    b
--
rotL (Node k l (Node rk rl rr)) = Node rk (Node k l rl) rr
rotL t = t

-- | Simple right rotation.
--
--      n2           n1
--     /  \         / \
--    n1   c   →   a   n2
--   /  \             /  \
--  a    b           b    c
--
rotR (Node k (Node lk ll lr) r) = Node lk ll (Node k lr r)
rotR t = t

-- | Left-right rotation.
rotLR (Node k (Node lk ll (Node lrk lrl lrr)) r) = Node lrk (Node lk ll lrl) (Node k lrr r)
rotLR t = t

-- | Right-left rotation.
rotRL (Node k l (Node rk (Node rlk rll rlr) rr)) = Node rlk (Node k l rll) (Node rk rlr rr)
rotRL t = t

-------------------------------------------------------------------------------

showPretty :: (Show k, Show v) => Tree k v -> String
showPretty tree =
    go 0 tree
  where
    go :: (Show k, Show v) => Int -> Tree k v -> String
    go lvl Empty = indent lvl ++ "Empty\n"
    go lvl (Node k l r) = concat $
        [ indent lvl, showNode k l r, "\n"
        , go (lvl + 2) l
        , go (lvl + 2) r
        ]
    go lvl (Leaf k v) =
        concat [indent lvl, "Leaf ", show k, " (val=", show v, ")\n"]

    indent lvl = concat (replicate lvl "  ")
    showNode k l r | node <- Node k l r =
        concat ["Node ", show k, " (height=", show (height node), ", ", show (balance node), ")"]

-- Instances ------------------------------------------------------------------

instance (Show k, Show v) => Show (Tree k v) where
    show = showPretty
