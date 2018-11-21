module Oscoin.Consensus.Class
    ( MonadQuery (..)

    -- * Re-exports
    , module Oscoin.Clock
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock

class Monad m => MonadQuery m where
    type Key m
    type Val m

    queryM :: Key m -> m (Maybe (Val m))

    default queryM
        :: (MonadQuery m', MonadTrans t, m ~ t m', Val m' ~ Val (t m'), Key m' ~ Key (t m'))
        => Key m -> m (Maybe (Val m))
    queryM = lift . queryM
    {-# INLINE queryM #-}
