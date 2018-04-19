module Crypto.Data.Auth.Tree
    ( Tree(..)
    , empty
    , member
    , insert
    , lookup
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
    , lookupMin
    , lookupMax
    , findMin
    , findMax
    , bounds
    , keys
    , size
    , flatten
    , merkleHash
    , emptyHash

    -- * Utility types and functions
    , Height
    , height

    , Balance
    , balance
    ) where

import           Crypto.Data.Auth.Tree.Internal

import           Crypto.Hash (HashAlgorithm, Digest)
import           Data.Binary (Binary)
import           Data.ByteArray (ByteArrayAccess)

import qualified Prelude
import           Prelude hiding (lookup, null, map, pred, succ)
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
    | Node !k !(Tree k v) !(Tree k v)
    | Leaf !k v
    deriving (Functor, Traversable, Foldable, Generic)

instance (Eq k, Eq v) => Eq (Tree k v) where
    (==) t1 t2 = toList t1 == toList t2

instance (Binary k, Binary v) => Binary (Tree k v)

-- Hashing --------------------------------------------------------------------

-- ! A few notes about merkle hashes.
--
-- * An empty tree hashes to the empty hash (all zeros).
-- * An inner node hashes to Hash(1 ∥ LeftHash ∥ RightHash).
-- * A leaf node hashes to Hash(0 ∥ Key ∥ Value).
--
-- The `0` and `1` prefixes are used to differentiate inner node from leaf node
-- hashes to protect against second pre-image attacks. Without the prefix, it's
-- possible to construct a leaf that has the same merkle hash as an inner node.
--
-- Given a node M with two children, L and R, the hash of M is
--
--      H(M) = H(H(L) ∥ H(R))
--
-- Now, given a leaf node M' constructed as such:
--
--      M' = Leaf(H(L), H(R))
--
-- where H(L) is the key and H(R) is the value, we have
--
--      H(M') = H(H(L) ∥ H(R))
--
-- and notice that H(M') = H(M), even though M and M' are different.
--

-- | Compute the merkle hash of a tree.
merkleHash
    :: (HashAlgorithm a, ByteArrayAccess k, ByteArrayAccess v)
    => Tree k v
    -> Digest a
merkleHash Empty        = emptyHash
merkleHash (Leaf k v)   = hashLeaf k v
merkleHash (Node _ l r) = hashNode (merkleHash l) (merkleHash r)

-------------------------------------------------------------------------------

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
{-# INLINABLE member #-}

-- | /O(log n)/. Insert a key and value into a tree.
insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert k v Empty = Leaf k v
insert k v (Node k' l r)
    | k < k'    = rebalance $ Node k' (insert k v l) r
    | otherwise = rebalance $ Node k' l (insert k v r)
insert k v (Leaf k' v')
    | k > k'    = rebalance $ Node k (Leaf k' v') (Leaf k v)
    | k < k'    = rebalance $ Node k' (Leaf k v) (Leaf k' v')
    | otherwise = Leaf k' v
{-# INLINABLE insert #-}

-- | /O(log n)/. Lookup a key from a tree.
lookup :: Ord k => k -> Tree k v -> Maybe v
lookup k (Leaf k' v) | k == k' = Just v
lookup k (Node k' l r)
    | k < k'    = lookup k l
    | otherwise = lookup k r
lookup _ _ = Nothing
{-# INLINABLE lookup #-}

-- | Get the predecessor of a key, or 'Nothing' if there is none.
pred :: Ord k => k -> Tree k v -> Maybe (k, v)
pred k (Node k' l r)
    | k == k'   = lookupMax l
    | k <  k'   = pred k l
    | otherwise = pred k r
pred k (Leaf k' v) | k' < k = Just (k', v)
pred _ _ = Nothing
{-# INLINABLE pred #-}

-- | Get the successor of a key, or 'Nothing' if there is none.
succ :: Ord k => k -> Tree k v -> Maybe (k, v)
succ k (Node k' l r)
    | k < k' =
        if fst (findMax l) <= k
           then lookupMin r
           else succ k l
    | otherwise = succ k r
succ k (Leaf k' v) | k' > k = Just (k', v)
succ _ _ = Nothing
{-# INLINABLE succ #-}

-- | Return the min and max value of a tree, or 'Nothing' if it's empty.
bounds :: Tree k v -> Maybe (k, k)
bounds tree = do
    (l, _) <- lookupMin tree
    (r, _) <- lookupMax tree
    pure (l, r)

-- | Lookup the minimum value of a tree, or 'Nothing' if it's empty.
lookupMin :: Tree k v -> Maybe (k, v)
lookupMin Empty = Nothing
lookupMin (Leaf k v) = Just (k, v)
lookupMin (Node _ l _) = lookupMin l

-- | Lookup the maximum value of a tree, or 'Nothing' if it's empty.
lookupMax :: Tree k v -> Maybe (k, v)
lookupMax Empty = Nothing
lookupMax (Leaf k v) = Just (k, v)
lookupMax (Node _ _ r) = lookupMax r

-- | Find the maximum value of a tree. Calls 'error' if the tree is empty.
findMax :: Tree k v -> (k, v)
findMax t
    | Just r <- lookupMax t = r
    | otherwise = error "Tree.findMax: empty tree has no maximal element"

-- | Find the minimum value of a tree. Calls 'error' if the tree is empty.
findMin :: Tree k v -> (k, v)
findMin t
    | Just r <- lookupMin t = r
    | otherwise = error "Tree.findMin: empty tree has no minimal element"

-- | /O(log n)/. The number of elements in the tree.
size :: Tree k v -> Int
size Empty        = 0
size (Leaf _ _)   = 1
size (Node _ l r) = size l + size r

-- | /O(log n)/. Delete a key from a tree.
delete :: Ord k => k -> Tree k v -> Tree k v
delete _ Empty = Empty
delete k leaf@(Leaf k' _)
    | k == k'   = Empty
    | otherwise = leaf
delete k tree =
    go tree
  where
    go (Node _ l (Leaf k' _)) | k' == k = l
    go (Node _ (Leaf k' _) r) | k' == k = r
    go (Node k' l r)
        | k < k'    = rebalance $ Node k' (go l) r
        | otherwise = rebalance $ Node leftmostKey l (go r)
      where
        leftmostKey = fst (findMin (go r))
    go tree = tree
{-# INLINABLE delete #-}

-- | Convert a tree into a list of @(k, v)@ pairs.
toList :: Tree k v -> [(k, v)]
toList Empty = []
toList (Leaf k v) = [(k, v)]
toList (Node _ l r) = toList l ++ toList r

-- | Create a tree from a list of key/value pairs.
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

-- | Return the height of a tree. If the tree is not balanced, returns the max
-- height between the left and right branch.
height :: Tree k v -> Height
height Empty        = 0
height (Leaf _ _)   = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- | Tree balance factor.
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

-- | The balance factor of a tree.
balance :: Tree k v -> Balance
balance Empty        = Balanced
balance (Leaf _ _)   = Balanced
balance (Node _ l r)
    | b < 0     = LeftHeavy  b
    | b > 0     = RightHeavy b
    | otherwise = Balanced
  where
    b = fromHeight (height r - height l)

-- | Rebalance a tree, such that the balance factor is between -1 and 1.
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
