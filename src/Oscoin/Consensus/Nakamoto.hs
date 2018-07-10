{-# LANGUAGE LambdaCase #-}

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
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashable, Hashed, hash)
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.P2P as P2P

import           Control.Monad.State (StateT, evalStateT, state)
import           Crypto.Number.Serialize (os2ip)
import           Data.Binary (Binary)
import           Data.Bool (bool)
import           Data.Functor (($>))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList)
import           Data.Traversable (for)
import           System.Random (StdGen, randomR)

epochLength :: Tick
epochLength = 1

score :: Blockchain tx -> Blockchain tx -> Ordering
score = comparing height

newtype NakamotoT tx m a = NakamotoT (StateT StdGen m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState StdGen
             )

instance MonadTrans (NakamotoT tx) where
    lift = NakamotoT . lift
    {-# INLINE lift #-}

instance MonadClock m => MonadClock (NakamotoT tx m) where
    currentTick = lift currentTick
    {-# INLINE currentTick #-}

instance ( MonadMempool    tx m
         , MonadBlockStore tx m
         , Hashable        tx
         ) => MonadProtocol tx (NakamotoT tx m)
  where
    stepM _ = \case
        P2P.BlockMsg blk -> do
            for_ (validateBlock blk) $ \blk' -> do
                BlockStore.storeBlock blk'
                delTxs (toList blk')
            pure mempty

        -- TODO: Validate tx.
        P2P.TxMsg txs ->
            addTxs txs $> mempty

        P2P.ReqBlockMsg blk ->
            map P2P.BlockMsg . maybeToList <$> BlockStore.lookupBlock blk

    tickM t = do
        blk <- shouldCutBlock >>= bool (pure Nothing) (mineBlock t)
        pure . maybeToList . map P2P.BlockMsg $ blk

    {-# INLINE stepM #-}
    {-# INLINE tickM #-}

runNakamotoT :: StdGen -> NakamotoT tx m a -> m (a, StdGen)
runNakamotoT rng (NakamotoT ma) = runStateT ma rng

evalNakamotoT :: Monad m => StdGen -> NakamotoT tx m a -> m a
evalNakamotoT rng (NakamotoT ma) = evalStateT ma rng

shouldCutBlock :: Monad m => NakamotoT tx m Bool
shouldCutBlock = do
    v <- state $ randomR (0, 1)
    pure $ v < p
  where
    p = 0.1 :: Float

mineBlock
    :: ( MonadMempool    tx m
       , MonadBlockStore tx m
       , Binary          tx
       )
    => Tick
    -> NakamotoT tx m (Maybe (Block tx))
mineBlock tick = do
    txs   <- map snd <$> getTxs
    chain <- BlockStore.maximumChainBy score
    let prevHash = hash . blockHeader $ tip chain
    for (findBlock tick prevHash minDifficulty txs) $ \blk -> do
        delTxs (toList blk)
        BlockStore.storeBlock blk
        pure blk

-- | Calculate block difficulty.
difficulty :: BlockHeader -> Difficulty
difficulty = os2ip . hash

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
hasPoW :: BlockHeader -> Bool
hasPoW header =
    difficulty header < blockDifficulty header

-- | Calculate the difficulty of a blockchain.
chainDifficulty :: Blockchain tx -> Difficulty
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

findPoW :: BlockHeader -> Maybe BlockHeader
findPoW bh@BlockHeader { blockNonce }
    | hasPoW bh =
        Just bh
    | blockNonce < (maxBound :: Word32) =
        findPoW bh { blockNonce = blockNonce + 1 }
    | otherwise =
        Nothing

findBlock
    :: (Binary tx, Foldable t)
    => Tick
    -> Hashed BlockHeader
    -> Difficulty
    -> t tx
    -> Maybe (Block tx)
findBlock t prevHash target txs = do
    header <- headerWithPoW
    pure $ mkBlock header txs
  where
    headerWithPoW    = findPoW headerWithoutPoW
    headerWithoutPoW = BlockHeader
        { blockPrevHash     = prevHash
        , blockRootHash     = hashTxs txs
        , blockDifficulty   = target
        , blockTimestamp    = toSecs t
        , blockNonce        = 0
        }
    toSecs = fromIntegral . fromEnum
