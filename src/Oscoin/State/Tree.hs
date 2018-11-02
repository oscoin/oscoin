module Oscoin.State.Tree where

import           Oscoin.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map

type Key = Text
type Path = [Text]
type Val = LBS.ByteString

-- | Key/value tree data-structure.
type Tree = Map Path Val

get :: Path -> Tree -> Maybe Val
get = Map.lookup

set :: Path -> Val -> Tree -> Tree
set = Map.insert
