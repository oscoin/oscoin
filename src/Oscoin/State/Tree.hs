module Oscoin.State.Tree where

import           Oscoin.Prelude
import qualified Data.Map as Map

type Key = Text
type Path = [Text]
type Val = LByteString

-- | Key/value tree data-structure.
type Tree k v = Map k v

get :: Ord k => k -> Tree k v -> Maybe v
get = Map.lookup

set :: Ord k => k -> v -> Tree k v -> Tree k v
set = Map.insert
