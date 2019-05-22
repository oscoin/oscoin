{-- | Little helpers geared towards making obvious in the types
      how blocks are ordered.

      The main idea here is /not/ to enforce some kind of ordering within
      the wrapped functor 'f', but rather to state a /contract/ in the API
      regarding which is the ordering of the elements.

      The typical example where these two newtypes are useful is when
      manipulating blocks: the 'BlockChain' data structure stores them
      newest-first (i.e. the `tip` is at the head) but sometimes is useful
      to have them in reverse order (for example when storing them in the
      `BlockStore`, where it's important to store them starting by the oldest
      first).
--}

module Oscoin.Time.Chrono
    ( NewestFirst(..)
    , OldestFirst(..)

    , reverse
    ) where

import           Codec.Serialise
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Oscoin.Prelude hiding (reverse)


-- | A simple typeclass which can \"talk\" about data structures which are
-- \"chronologically ordered\", and as such can be reversed. We offer implementations
-- for both `NewestFirst` and `OldestFirst` parametrised over `[]` and `NonEmpty`.
-- This allows for quickly reversing the order while preserving a structure:
--
-- >>> toNewestFirst . reverse . OldestFirst $ [1,2,3]
-- [3,2,1]
--
class ChronogicallyOrdered a where
    type ReverseOf a :: *
    reverse :: a -> ReverseOf a

-- | A wrapper over a 'Foldable' functor 'f', typically a list or a non-empty
-- one, where the API contract stats that elements are ordered from the newest
-- to the oldest.
newtype NewestFirst f a = NewestFirst { toNewestFirst :: f a }
  deriving (Show, Eq, Functor, Foldable, Semigroup, Monoid)

instance Serialise (f a) => Serialise (NewestFirst f a) where
    encode = encode . toNewestFirst
    decode = map NewestFirst decode

-- | A wrapper over a 'Foldable' functor 'f', typically a list or a non-empty
-- one, where the API contract stats that elements are ordered from the oldest
-- to the newest.
newtype OldestFirst f a = OldestFirst { toOldestFirst :: f a }
  deriving (Show, Eq, Functor, Foldable, Semigroup, Monoid)

instance Serialise (f a) => Serialise (OldestFirst f a) where
    encode = encode . toOldestFirst
    decode = map OldestFirst decode

instance ChronogicallyOrdered (NewestFirst [] a) where
    type ReverseOf (NewestFirst [] a) = OldestFirst [] a
    reverse (NewestFirst a) = OldestFirst (List.reverse a)

instance ChronogicallyOrdered (OldestFirst [] a) where
    type ReverseOf (OldestFirst [] a) = NewestFirst [] a
    reverse (OldestFirst a) = NewestFirst (List.reverse a)

instance ChronogicallyOrdered (NewestFirst NonEmpty a) where
    type ReverseOf (NewestFirst NonEmpty a) = OldestFirst NonEmpty a
    reverse (NewestFirst a) = OldestFirst (NonEmpty.reverse a)

instance ChronogicallyOrdered (OldestFirst NonEmpty a) where
    type ReverseOf (OldestFirst NonEmpty a) = NewestFirst NonEmpty a
    reverse (OldestFirst a) = NewestFirst (NonEmpty.reverse a)
