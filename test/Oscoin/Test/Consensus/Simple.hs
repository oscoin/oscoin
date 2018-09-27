module Oscoin.Test.Consensus.Simple
    ( SimpleT
    , LastTime(..)

    , runSimpleT

    , epochLength
    , chainScore
    , shouldCutBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (Tick)
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..))
import           Oscoin.Consensus.Evaluator
import           Oscoin.Consensus.Mining (mineBlock)
import           Oscoin.Consensus.Simple
import           Oscoin.Node.Mempool.Class (MonadMempool(..))

import           Oscoin.Test.Consensus.Class (MonadProtocol(..))

import           Codec.Serialise (Serialise)
import           Control.Monad.State (modify')

type Position = (Int, Int)

data LastTime = LastTime
    { ltLastBlk :: Tick
    , ltLastAsk :: Tick
    } deriving Show

newtype SimpleT tx i m a = SimpleT (ReaderT Position (StateT LastTime m) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Position
             , MonadState  LastTime
             )

instance MonadTrans (SimpleT tx i) where
    lift = SimpleT . lift . lift
    {-# INLINE lift #-}

instance Monad m => MonadLastTime (SimpleT tx i m) where
    getLastBlockTick      = gets ltLastBlk
    setLastBlockTick tick = modify' (\s -> s { ltLastBlk = tick })

    getLastAskTick        = gets ltLastAsk
    setLastAskTick tick   = modify' (\s -> s { ltLastAsk = tick })

instance MonadMempool    tx    m => MonadMempool    tx    (SimpleT tx i m)
instance MonadBlockStore tx () m => MonadBlockStore tx () (SimpleT tx i m)

runSimpleT :: Position -> LastTime -> SimpleT tx i m a -> m (a, LastTime)
runSimpleT env lt (SimpleT ma) = runStateT (runReaderT ma env) lt

instance ( MonadMempool    tx    m
         , MonadBlockStore tx () m
         , Serialise       tx
         ) => MonadProtocol tx () (SimpleT tx i m)
  where
    mineM  tick = ask >>= \pos -> mineBlock (simpleConsensus pos) identityEval tick
    reconcileM  = reconcileSimple
