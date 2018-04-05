module Oscoin.Consensus.Class where

import           Oscoin.Prelude

class Context m where
  type State m
  type Key m

  get :: Key m -> m (Maybe ByteString)
  set :: Key m -> ByteString -> m (State m)
  del :: Key m -> m (State m)

type Score = [ByteString]

class View m where
  type Transaction m
  type BlockHeader m

  apply :: Maybe (BlockHeader m) -> [Transaction m] -> m (State m)

class Protocol a where
  type Msg a

  step :: a -> Msg a -> (a, [Msg a])
