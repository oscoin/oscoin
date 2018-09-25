module Oscoin.Test.Consensus.Simple
    ( Env
    , LastTime (..)
    , SimpleT

    , mkEnv
    , runSimpleT

    , epochLength
    , chainScore
    , shouldCutBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (Tick)
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..))
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.Consensus.Evaluator
import           Oscoin.Consensus.Mining (mineBlock)
import           Oscoin.Consensus.Simple
import           Oscoin.Crypto.Hash (Hashable)
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Storage as Storage

import           Oscoin.Test.Consensus.Class (MonadProtocol(..), Msg(..))

import           Codec.Serialise (Serialise)
import           Control.Monad.State (modify')
import           Data.Maybe (maybeToList)
import           Lens.Micro (lens)

data Env i = Env
    { envSelf  :: i
    , envPeers :: Set i
    } deriving Show

instance HasSelf (Env i) i where
    self = lens envSelf (\s a -> s { envSelf = a })
    {-# INLINE self #-}

instance HasPeers (Env i) i where
    peers = lens envPeers (\s a -> s { envPeers = a })
    {-# INLINE peers #-}

mkEnv :: i -> Set i -> Env i
mkEnv = Env

data LastTime = LastTime
    { ltLastBlk :: Tick
    , ltLastAsk :: Tick
    } deriving Show

newtype SimpleT tx i m a = SimpleT (ReaderT (Env i) (StateT LastTime m) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (Env i)
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

runSimpleT :: Env i -> LastTime -> SimpleT tx i m a -> m (a, LastTime)
runSimpleT env lt (SimpleT ma) = runStateT (runReaderT ma env) lt

instance ( MonadMempool    tx    m
         , MonadBlockStore tx () m
         , Serialise       tx
         , Hashable        tx
         , Ord i
         ) => MonadProtocol tx () (SimpleT tx i m)
  where
    stepM _ msg = respond identityEval msg
    mineM  tick = ask >>= \e -> mineBlock (simpleConsensus e) identityEval tick
    reconcileM  = reconcileSimple

respond
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       , Hashable tx
       )
    => Evaluator s tx ()
    -> Msg tx
    -> m [Msg tx]
respond eval msg = go msg
  where
    go (TxMsg tx)        = mempty <$ Storage.applyTx tx
    go (BlockMsg blk)    = mempty <$ Storage.applyBlock eval blk
    go (ReqBlockMsg blk) = do
        mblk <- BlockStore.lookupBlock blk
        pure . maybeToList . map (BlockMsg . void) $ mblk
