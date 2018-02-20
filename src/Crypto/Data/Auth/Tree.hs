module Crypto.Data.Auth.Tree
    ( Tree(..)
    , empty
    , elem
    , insert
    , lookup
    , delete
    , null
    , toList
    , fromList

    -- * Utility types and functions
    , Height
    , height

    , Balance
    , balance
    ) where

import           Prelude hiding (lookup, null, elem)

-- * Note: The key of an inner node is always the left-most key of its right
--   sub-tree.

data Tree k v =
      Empty
    | Node k (Tree k v) (Tree k v)
    | Leaf k v
    deriving (Show, Eq)

empty :: Tree k v
empty = Empty

elem :: Ord k => k -> Tree k v -> Bool
elem _ Empty = False
elem k (Leaf k' _) = k == k'
elem k (Node k' l r)
    | k < k'    = elem k l
    | otherwise = elem k r

insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert k v Empty = Leaf k v
insert k v (Node k' l r)
    | k < k'    = Node k' (insert k v l) r
    | otherwise = Node k' l (insert k v r)
insert k v (Leaf k' v')
    | k == k' = Leaf k' v
    | k >  k' = Node k (Leaf k' v') (Leaf k v)
    | k <  k' = Node k' (Leaf k v) (Leaf k' v')
insert _ _ _ =
    undefined

lookup :: Ord k => k -> Tree k v -> Maybe v
lookup k (Leaf k' v) | k == k' = Just v
lookup k (Node k' l r)
    | k < k'    = lookup k l
    | otherwise = lookup k r
lookup _ _ = Nothing

delete :: Ord k => k -> Tree k v -> Tree k v
delete _ Empty = Empty
delete k leaf@(Leaf k' _)
    | k == k'   = Empty
    | otherwise = leaf
delete k (Node k' l r)
    | k < k'    = Node k' (delete k l) r
    | otherwise = Node k' l (delete k r)

toList :: Tree k v -> [(k, v)]
toList Empty = []
toList (Leaf k v) = [(k, v)]
toList (Node _ l r) = toList l ++ toList r

fromList :: Ord k => [(k, v)] -> Tree k v
fromList kvs = foldr (\(k, v) tree -> insert k v tree) Empty kvs

null :: Tree k v -> Bool
null Empty = True
null _     = False

-- Utility --------------------------------------------------------------------

newtype Height = Height { fromHeight :: Int }
    deriving (Num, Eq, Ord, Show)

height :: Tree k v -> Height
height Empty        = 0
height (Leaf _ _)   = 0
height (Node _ l r) = max (height l + 1) (height r + 1)

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
