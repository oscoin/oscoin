module Oscoin.Consensus.Class where

import           Oscoin.Prelude

class Context m where
    type T m
    type Key m

    get :: Key m -> m (Maybe ByteString)
    set :: Key m -> ByteString -> m (T m)
    del :: Key m -> m (T m)

type Score = [ByteString]

class View m where
    type Transaction m
    type BlockHeader m

    apply :: Maybe (BlockHeader m) -> [Transaction m] -> m ()

class (Ord (Tick a), Num (Tick a)) => Protocol a where
    type Msg a
    type Addr a
    type Tick a

    step :: a -> Tick a -> Maybe (Addr a, Msg a) -> (a, [(Addr a, Msg a)])
    epoch :: a -> Tick a
