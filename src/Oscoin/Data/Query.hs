module Oscoin.Data.Query where

import           Oscoin.Prelude

class Query a where
    query :: ByteString -> a -> Maybe ByteString
