{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Consensus.Class
    ( Score
    , MonadFold (..)
    , MonadQuery (..)
    , MonadModify (..)
    , MonadProtocol (..)

    -- * Re-exports
    , module Oscoin.Clock
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock
import qualified Oscoin.P2P as P2P

type Score = ByteString

class Monad m => MonadFold m where
    type Op        m :: *
    type OpContext m :: *

    -- | State transition function.
    --
    -- Apply a sequence of operations to the \"world state\"
    foldM :: Foldable t => Maybe (OpContext m) -> t (Op m) -> m ()

class Monad m => MonadQuery m where
    type Key m
    type Val m

    queryM :: Key m -> m (Maybe (Val m))

class MonadQuery m => MonadModify m where
    setM :: Key m -> Val m -> m ()
    delM :: Key m -> m ()

class Monad m => MonadProtocol tx m | m -> tx where
    stepM :: Tick -> P2P.Msg tx -> m [P2P.Msg tx]
    tickM :: Tick -> m [P2P.Msg tx]

instance {-# OVERLAPPABLE #-}
    ( MonadTrans    t
    , Monad         (t m)
    , MonadProtocol tx m
    ) => MonadProtocol tx (t m)
  where
    stepM t = lift . stepM t
    tickM   = lift . tickM
    {-# INLINE stepM #-}
    {-# INLINE tickM #-}

