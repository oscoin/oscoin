module Oscoin.Consensus.Class where

import           Oscoin.Prelude

import           Data.Time.Clock (NominalDiffTime)

class Context m where
    type T m
    type Key m

    get :: Key m -> m (Maybe ByteString)
    set :: Key m -> ByteString -> m (T m)
    del :: Key m -> m (T m)

type Score = [ByteString]
type Tick  = NominalDiffTime

class View m where
    type Transaction m
    type TransactionContext m

    apply :: Maybe (TransactionContext m) -> [Transaction m] -> m ()

class Ord (Addr a) => Protocol a where
    type Msg a
    type Addr a

    step :: a -> Tick -> Maybe (Addr a, Msg a) -> (a, [(Addr a, Msg a)])
    epoch :: a -> Tick
