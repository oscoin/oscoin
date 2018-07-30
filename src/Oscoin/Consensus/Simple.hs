{-# LANGUAGE LambdaCase #-}

module Oscoin.Consensus.Simple
    ( Env
    , LastTime (..)
    , SimpleT

    , mkEnv

    , runSimpleT
    , evalSimpleT

    , epochLength

    , chainScore

    , shouldCutBlock
    , shouldReconcile
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..))
import           Oscoin.Consensus.Class (MonadProtocol(..), Tick)
import           Oscoin.Consensus.Evaluator
import           Oscoin.Crypto.Blockchain (Blockchain, height, tip)
import           Oscoin.Crypto.Blockchain.Block (Block(..), BlockHeader(..), mkBlock, emptyHeader, validateBlock, toOrphan, hashTxs)
import           Oscoin.Crypto.Hash
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.P2P as P2P

import           Control.Monad.State
import           Data.Binary (Binary)
import           Data.Bool (bool)
import           Data.Functor (($>))
import           Data.Maybe (maybeToList)
import qualified Data.Set as Set

epochLength :: Tick
epochLength = 1

data Env i = Env
    { envSelf  :: i
    , envPeers :: Set i
    } deriving Show

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

instance ( MonadMempool    tx    m
         , MonadBlockStore tx () m
         , Binary          tx
         , Ord i
         ) => MonadProtocol tx (SimpleT tx i m)
  where
    stepM _ = \case
        P2P.BlockMsg blk -> do
            for_ (validateBlock blk) $ \blk' -> do
                storeBlock $ toOrphan acceptAnythingEval blk
                delTxs (blockData blk')
            pure mempty

        -- TODO: Validate tx.
        P2P.TxMsg txs ->
            addTxs txs $> mempty

        P2P.ReqBlockMsg blk ->
            map P2P.BlockMsg . maybeToList <$> lookupBlock blk

    tickM tick = do
        blks <-
            shouldCutBlockM tick >>= bool (pure mempty) (do
                txs   <- map snd <$> getTxs
                chain <- maximumChainBy (comparing chainScore)

                let prevHash  = hash . blockHeader $ tip chain
                    timestamp = fromIntegral (fromEnum tick)
                    blk       = mkBlock header txs
                    header    = emptyHeader { blockTimestamp = timestamp
                                            , blockPrevHash  = prevHash
                                            , blockDataHash  = hashTxs txs }

                storeBlock $ toOrphan acceptAnythingEval blk
                delTxs (blockData blk)
                modify' (\s -> s { ltLastBlk = tick })

                pure [P2P.BlockMsg blk])

        reqs <-
            shouldReconcileM tick >>= bool (pure mempty) (do
                modify' (\s -> s { ltLastAsk = tick })
                map P2P.ReqBlockMsg . toList <$> orphans)

        pure $ blks <> reqs

    {-# INLINE stepM #-}
    {-# INLINE tickM #-}

instance P2P.MonadNetwork tx    m => P2P.MonadNetwork tx    (SimpleT tx i m)
instance MonadMempool     tx    m => MonadMempool     tx    (SimpleT tx i m)
instance MonadBlockStore  tx () m => MonadBlockStore  tx () (SimpleT tx i m)

mkEnv :: i -> Set i -> Env i
mkEnv = Env

runSimpleT :: Env i -> LastTime -> SimpleT tx i m a -> m (a, LastTime)
runSimpleT env lt (SimpleT ma) = runStateT (runReaderT ma env) lt

evalSimpleT :: Monad m => Env i -> LastTime -> SimpleT tx i m a -> m a
evalSimpleT env lt (SimpleT ma) = evalStateT (runReaderT ma env) lt

chainScore :: Blockchain tx s -> Int
chainScore bc =
    (bigMagicNumber * h) - steps
  where
    h              = height bc
    lastBlock      = tip bc
    timestampNs    = blockTimestamp $ blockHeader lastBlock
    timestamp      = timestampNs `div` 1000000000000
    e              = round epochLength
    steps          = fromIntegral timestamp `div` e :: Int
    bigMagicNumber = 2526041640 -- some loser in 2050 has to deal with this bug

shouldReconcileM :: Monad m => Tick -> SimpleT tx i m Bool
shouldReconcileM at = do
    lastAsk <- gets ltLastAsk
    pure $ shouldReconcile lastAsk at

shouldReconcile :: Tick -> Tick -> Bool
shouldReconcile lastAsk at = time - round lastAsk >= stepTime
  where
    time     = round at :: Int
    stepTime = round epochLength

shouldCutBlockM :: (Monad m, Ord i) => Tick -> SimpleT tx i m Bool
shouldCutBlockM at = SimpleT $ do
    lastBlk <- gets ltLastBlk
    self    <- asks envSelf
    peers   <- asks envPeers
    pure $ shouldCutBlock lastBlk self peers at

shouldCutBlock :: Ord i => Tick -> i -> Set i -> Tick -> Bool
shouldCutBlock lastBlk self peers at = beenAWhile && ourTurn
  where
    time              = round at
    stepTime          = round epochLength
    nTotalPeers       = 1 + Set.size peers
    relativeBlockTime = stepTime * nTotalPeers
    beenAWhile        = time - round lastBlk >= relativeBlockTime
    stepNumber        = time `div` stepTime
    ourOffset         = Set.size $ Set.filter (< self) peers
    currentOffset     = stepNumber `mod` nTotalPeers
    ourTurn           = currentOffset == ourOffset
