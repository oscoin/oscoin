module Oscoin.State.Tree where

import           Oscoin.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as T

type Key = Text
type Path = [Text]
type Val = LBS.ByteString

keyToPath :: Key -> Path
keyToPath = T.splitOn "/"

-- | Key/value tree data-structure.
type Tree k v = Map k v

get :: Ord k => k -> Tree k v -> Maybe v
get = Map.lookup

set :: Ord k => k -> v -> Tree k v -> Tree k v
set = Map.insert
