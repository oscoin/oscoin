module Oscoin.Data.Query where

import Oscoin.Prelude

class Query a where
    type QueryVal a :: *

    query :: [Text] -> a -> Maybe (QueryVal a)
