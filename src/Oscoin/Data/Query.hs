module Oscoin.Data.Query where

import           Oscoin.Prelude

import qualified Data.Map as Map

class Query a where
    type QueryVal a :: *

    query :: [Text] -> a -> Maybe (QueryVal a)

instance Query (Map [Text] v) where
    type QueryVal (Map [Text] v) = v

    query = Map.lookup
