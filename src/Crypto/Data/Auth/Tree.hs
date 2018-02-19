module Crypto.Data.Auth.Tree
    ( Tree(..)
    , empty
    , insert
    , lookup
    , delete
    , null
    , toList
    , fromList
    ) where

import           Prelude hiding (lookup, null)

-- * Note: The key of an inner node is always the left-most key of its right
--   sub-tree.

data Tree k v =
      Empty
    | Node k (Tree k v) (Tree k v)
    | Leaf k v
    deriving (Show, Eq)

empty :: Tree k v
empty = Empty

insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert k v Empty = Leaf k v
insert k v (Node k' l r)
    | k < k'    = Node k' (insert k v l) r
    | otherwise = Node k' l (insert k v r)
insert k v (Leaf k' _) | k == k' =
    Leaf k' v
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
toList _ = undefined

fromList :: Ord k => [(k, v)] -> Tree k v
fromList kvs = foldr (\(k, v) tree -> insert k v tree) Empty kvs

null :: Tree k v -> Bool
null Empty = True
null _     = False
