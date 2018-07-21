{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Consensus.Nakamoto
    ( NakamotoT

    , runNakamotoT
    , evalNakamotoT

    , epochLength
    , score

    , difficulty
    , defaultGenesisDifficulty
    , chainDifficulty
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Evaluator
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash (Hashable, Hashed, hash, zeroHash)
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.P2P as P2P

import           Control.Monad.RWS (RWST, evalRWST, runRWST, state)
import           Crypto.Number.Serialize (os2ip)
import           Data.Bifunctor (second)
import           Data.Binary (Binary)
import           Data.Bool (bool)
import           Data.Functor (($>))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList, catMaybes)
import           Data.Traversable (for)
import           System.Random (StdGen, randomR)

epochLength :: Tick
epochLength = 1

type NakamotoEval tx s = Evaluator s tx ()

newtype NakamotoT tx s m a = NakamotoT (RWST (NakamotoEval tx s) () StdGen m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadEval tx s
             , MonadWriter ()
             , MonadState StdGen
             )

score :: Blockchain tx s -> Blockchain tx s -> Ordering
score = comparing height

type MonadEval tx s = MonadReader (NakamotoEval tx s)

instance MonadTrans (NakamotoT tx s) where
    lift = NakamotoT . lift
    {-# INLINE lift #-}

instance MonadClock m => MonadClock (NakamotoT tx s m) where
    currentTick = lift currentTick
    {-# INLINE currentTick #-}

instance ( MonadMempool    tx   m
         , MonadBlockStore tx s m
         , Hashable        tx
         , Binary          tx
         ) => MonadProtocol tx (NakamotoT tx s m)
  where
    stepM _ = \case
        P2P.BlockMsg blk ->
            isNovelBlock (blockHash blk) >>= \case
                True | Right blk' <- validateBlock blk -> do
                    evalFn <- ask
                    BlockStore.storeBlock $ toOrphan evalFn blk'
                    delTxs (blockData blk')
                    pure [P2P.BlockMsg blk']
                _ ->
                    pure mempty

        -- TODO: Validate tx.
        P2P.TxMsg txs ->
            map catMaybes $ for txs $ \tx -> do
                n <- isNovelTx (hash tx)
                if n then addTxs [tx] $> Just (P2P.TxMsg [tx])
                     else pure Nothing

        P2P.ReqBlockMsg blk -> do
            mblk <- BlockStore.lookupBlock blk
            pure . maybeToList . map (P2P.BlockMsg . second (const ())) $ mblk

    tickM t = do
        e    <- ask
        mblk <- shouldCutBlock >>= bool (pure Nothing) (mineBlock t e)
        pure . maybeToList . map (P2P.BlockMsg . second (const ())) $ mblk

    {-# INLINE stepM #-}
    {-# INLINE tickM #-}

isNovelBlock :: (MonadBlockStore tx s m) => BlockHash -> m Bool
isNovelBlock h =
    isNothing <$> BlockStore.lookupBlock h

isNovelTx :: (MonadBlockStore tx s m, MonadMempool tx m) => Hashed tx -> m Bool
isNovelTx h = do
    inBlockStore <- BlockStore.lookupTx h
    inMempool    <- lookupTx h
    pure . isNothing $ inBlockStore <|> inMempool

runNakamotoT :: StdGen -> NakamotoT tx s m a -> m (a, StdGen, ())
runNakamotoT rng (NakamotoT ma) = runRWST ma acceptAnythingEval rng

evalNakamotoT :: Monad m => StdGen -> NakamotoT tx s m a -> m (a, ())
evalNakamotoT rng (NakamotoT ma) = evalRWST ma acceptAnythingEval rng

shouldCutBlock :: Monad m => NakamotoT s tx m Bool
shouldCutBlock = do
    v <- state $ randomR (0, 1)
    pure $ v < p
  where
    p = 0.1 :: Float

mineBlock
    :: ( MonadMempool    tx   m
       , MonadBlockStore tx s m
       , Binary          tx
       )
    => Tick
    -> NakamotoEval tx s
    -> NakamotoT    tx s m (Maybe (Block tx s))
mineBlock tick evalFn = do
    txs    <- map snd <$> getTxs
    parent <- tip <$> BlockStore.maximumChainBy (comparing height)

    let (results, s') = applyValidExprs txs (blockState . blockHeader $ parent) evalFn
    case map fst (rights results) of
        [] ->
            pure Nothing
        validTxs ->
            for (findBlock tick (blockHash parent) s' minDifficulty validTxs) $ \blk -> do
                delTxs (blockData blk)
                BlockStore.storeBlock $ map (const . Just) blk
                pure blk

-- | Calculate block difficulty.
difficulty :: BlockHeader s -> Difficulty
difficulty = os2ip . headerHash

-- | The minimum difficulty.
minDifficulty :: Difficulty
minDifficulty =
    0xEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | The default difficulty at genesis.
defaultGenesisDifficulty :: Difficulty
defaultGenesisDifficulty =
    0x00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    -- This is the original difficulty of Bitcoin at genesis.

-- | Return whether or not a block has a valid difficulty.
hasPoW :: BlockHeader s -> Bool
hasPoW header =
    difficulty header < blockDifficulty header

-- | Calculate the difficulty of a blockchain.
chainDifficulty :: Blockchain tx s -> Difficulty
chainDifficulty (Blockchain blks) =
    if   length range < blocksConsidered
    then genesisDifficulty
    else currentDifficulty * targetElapsed `div` toInteger actualElapsed
  where
    range             = NonEmpty.take blocksConsidered blks
    rangeStart        = blockHeader $ NonEmpty.last (NonEmpty.head blks :| tail range)
    rangeEnd          = blockHeader $ NonEmpty.head blks
    actualElapsed     = blockTimestamp rangeEnd - blockTimestamp rangeStart
    targetElapsed     = fromIntegral $ blocksConsidered * blockTimeSeconds
    blocksConsidered  = timePeriodMinutes `div` blockTimeMinutes
    timePeriodMinutes = timePeriodDays * 24 * 60
    timePeriodDays    = 2 * 7 -- Two weeks.
    blockTimeMinutes  = 10
    blockTimeSeconds  = blockTimeMinutes * 60
    currentDifficulty = blockDifficulty rangeEnd
    genesisDifficulty = blockDifficulty . blockHeader $ NonEmpty.last blks

findPoW :: BlockHeader () -> Maybe (BlockHeader ())
findPoW bh@BlockHeader { blockNonce }
    | hasPoW bh =
        Just bh
    | blockNonce < (maxBound :: Word32) =
        findPoW bh { blockNonce = blockNonce + 1 }
    | otherwise =
        Nothing

findBlock
    :: (Foldable t, Binary tx)
    => Tick
    -> BlockHash
    -> s
    -> Difficulty
    -> t tx
    -> Maybe (Block tx s)
findBlock t parent st target txs = do
    header <- headerWithPoW
    pure $ second (const st) $ mkBlock header txs
  where
    headerWithPoW    = findPoW headerWithoutPoW
    headerWithoutPoW = BlockHeader
        { blockPrevHash     = parent
        , blockDataHash     = hashTxs txs
        , blockState        = ()
        , blockStateHash    = zeroHash
        , blockDifficulty   = target
        , blockTimestamp    = toSecs t
        , blockNonce        = 0
        }
    toSecs = fromIntegral . fromEnum
