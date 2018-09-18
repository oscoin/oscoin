{-# LANGUAGE DefaultSignatures #-}

module Oscoin.Consensus.Class
    ( Score
    , MonadQuery (..)
    , MonadUpdate (..)
    , MonadProtocol (..)

    -- * Re-exports
    , module Oscoin.Clock
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock
import qualified Oscoin.P2P as P2P

type Score = ByteString

class Monad m => MonadQuery m where
    type Key m
    type Val m

    queryM :: Key m -> m (Maybe (Val m))

    default queryM
        :: (MonadQuery m', MonadTrans t, m ~ t m', Val m' ~ Val (t m'), Key m' ~ Key (t m'))
        => Key m -> m (Maybe (Val m))
    queryM = lift . queryM
    {-# INLINE queryM #-}

class Monad m => MonadUpdate s m | m -> s where
    updateM :: s -> m ()

    default updateM
        :: (MonadUpdate s m', MonadTrans t, m ~ t m')
        => s -> m ()
    updateM s = lift $ updateM s
    {-# INLINE updateM #-}

class Monad m => MonadProtocol tx m | m -> tx where
    stepM :: Tick -> P2P.Msg tx -> m [P2P.Msg tx]
    tickM :: Tick -> m [P2P.Msg tx]

    default stepM
        :: (MonadProtocol tx m', MonadTrans t, m ~ t m')
        => Tick -> P2P.Msg tx -> m [P2P.Msg tx]
    stepM t = lift . stepM t
    {-# INLINE stepM #-}

    default tickM
        :: (MonadProtocol tx m', MonadTrans t, m ~ t m')
        => Tick -> m [P2P.Msg tx]
    tickM = lift . tickM
    {-# INLINE tickM #-}
