-- | A thin wrapper around a strict 'Map' from the @containers@ package that
-- keeps track of its elements via an internal reference count.
--
-- In order to manipulate this collection, only two functions are exported:
-- calling 'insert' would replace (or insert) the element @a@ at the key @k@
-- while incrementing its internal count, and calling 'delete' would /actually/
-- delete the element only if the reference count goes down to 0.

module Data.Map.RefCounted
    ( RefCounted

    , empty
    , size
    , insert
    , delete
    , keysSet
    , member
    ) where

import           Prelude

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)

type RefCount = Int
newtype RefCounted k v = RefCounted (Map k (v, RefCount))

{------------------------------------------------------------------------------
   Construction
------------------------------------------------------------------------------}

-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: RefCounted k v
empty = RefCounted $ M.empty
{-# INLINE empty #-}


insert :: Ord k => k -> v -> RefCounted k v -> RefCounted k v
insert k v (RefCounted mp) = RefCounted $
    M.insertWith (\(new,_) (_,!count) -> (new, succ count)) k (v, 1) mp
{-# INLINE insert #-}

{------------------------------------------------------------------------------
    Queries
------------------------------------------------------------------------------}

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: RefCounted k v -> Int
size (RefCounted m) = M.size m

-- | /O(n)/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [3,5]
-- > keysSet empty == Data.Set.empty
keysSet :: RefCounted k v -> Set k
keysSet (RefCounted m) = M.keysSet m
{-# INLINE keysSet #-}


-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: Ord k => k -> RefCounted k a -> Bool
member k (RefCounted m) = M.member k m
{-# INLINE member #-}

{------------------------------------------------------------------------------
    Deletion
------------------------------------------------------------------------------}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

delete :: Ord k => k -> RefCounted k v -> RefCounted k v
delete k (RefCounted mp) = RefCounted $
    case M.lookup k mp of
        Nothing      -> mp
        Just (_, 1)  -> M.delete k mp
        Just (v, !c) -> M.insert k (v, pred c) mp
{-# INLINE delete #-}
