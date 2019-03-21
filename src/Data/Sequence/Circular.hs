{--
 This module models a circular buffer using a vector as a foundation.
--}
module Data.Sequence.Circular (
    CircularSeq
  , empty
  , toList
  , (<|)
  , getLimit
  , getSize
  )
  where

import           GHC.Natural
import           Prelude

import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Mutable as MV

{------------------------------------------------------------------------------
  Types and instances
------------------------------------------------------------------------------}

type Limit = Int
type Size  = Int

-- | The index to the head of the vector representing the sequence.
-- This \"pointer\" starts at the end of the vector and \"wraps\" when we
-- reach 0. This ensure we can implement 'toList' efficiently by avoiding
-- needless calls to 'reverse'.
type Head  = Int

data CircularSeq a =
    CSeq                 Limit
          {-# UNPACK #-} Size
          {-# UNPACK #-} Head
          (V.Vector a)

instance Show a => Show (CircularSeq a) where
   show = show . toList

{------------------------------------------------------------------------------
  Main API
------------------------------------------------------------------------------}

-- | /O(n)/ Converts the 'CircularSeq' into a list. The order of the elements
-- is dictated by which operations was used to add elements to the 'CircularSeq'.
--
-- For example, inserting elements with (<|) means the final list will have
-- the elements ordered from the newest to the oldest:
-- >>> toList (4 <| (3 <| (2 <| (1 <| empty 3))))
-- [4,3,2]
toList :: CircularSeq a -> [a]
toList (CSeq limit sz hd v)
  | sz == 0   = []
  | sz < limit  = F.toList $ GV.unsafeSlice hd sz v
  | otherwise   = F.toList $ GV.unsafeSlice hd (sz - hd) v <> GV.unsafeSlice 0 hd v

-- /O(1)/ Gets the limit of the sequence.
getLimit :: CircularSeq a -> Limit
getLimit (CSeq limit _ _ _) = limit

-- /O(1)/ Gets the current size of the sequence.
getSize :: CircularSeq a -> Size
getSize (CSeq _ size _ _) = size

-- | Initialises an empty 'CircularSeq', given the max capacity.
-- Trying to initialise a 'CircularSeq' with a capacity of 0 would result in
-- a runtime error.
empty :: Natural -> CircularSeq a
empty (fromIntegral -> limit)
    | limit == 0 = error "Data.Sequence.Circular: the sequence cannot have a limit of 0."
    | otherwise = let v = GV.create (MV.unsafeNew limit) in CSeq limit 0 limit v

infixr 5 <|

-- | Appends the element at the beginning of the sequence.
(<|) :: a -> CircularSeq a -> CircularSeq a
x <| (CSeq limit sz hd xs ) =
    let hd' = if hd - 1 < 0 then limit - 1 else hd - 1
        v' = GV.modify (\v -> MV.write v hd' x) xs
    in CSeq limit (min limit (sz + 1)) hd' v'
