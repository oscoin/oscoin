{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Consensus.Nakamoto
    ( NakamotoT
    , NakamotoEnv(..)

    , runNakamotoT
    , evalNakamotoT

    , epochLength
    , score

    , difficulty
    , minDifficulty
    , easyDifficulty
    , defaultGenesisDifficulty
    , chainDifficulty

    , mineBlock
    , mineBlockRandom
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
import           Data.Functor (($>))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList, catMaybes)
import           Data.Traversable (for)
import           System.Random (StdGen, randomR, split)

epochLength :: Tick
epochLength = 1

type NakamotoEval tx s = Evaluator s tx ()

-- | A Nakamoto mining function. Tries all nonces and returns 'Nothing' if
-- no block satisfying the difficulty was found.
type NakamotoMiner tx s =
       Tick              -- ^ Current time
    -> StdGen            -- ^ A random number generator
    -> NakamotoEval tx s -- ^ An evaluation function for extrinsics
    -> Difficulty        -- ^ Target difficulty
    -> [tx]              -- ^ Transactions to include in the block
    -> Block tx s        -- ^ Parent block to build upon
    -> Maybe (Block tx s)

-- | Read-only environment for the Nakamoto consensus protocol.
data NakamotoEnv tx s = NakamotoEnv
    { nakEval       :: NakamotoEval tx s
    -- ^ Evaluation function to use for extrinsics
    , nakDifficulty :: Difficulty
    -- ^ Target difficulty (Nb. this is fixed for now and does not adjust)
    , nakMiner      :: NakamotoMiner tx s
    -- ^ Mining function to use
    }

instance Binary tx => Default (NakamotoEnv tx s) where
    def = NakamotoEnv
        { nakEval = acceptAnythingEval
        , nakDifficulty = easyDifficulty
        , nakMiner = mineBlock
        }

newtype NakamotoT tx s m a = NakamotoT (RWST (NakamotoEnv tx s) () StdGen m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader (NakamotoEnv tx s)
             , MonadWriter ()
             , MonadState StdGen
             )

score :: Blockchain tx s -> Blockchain tx s -> Ordering
score = comparing height

instance MonadTrans (NakamotoT tx s) where
    lift = NakamotoT . lift
    {-# INLINE lift #-}

instance MonadClock m => MonadClock (NakamotoT tx s m) where
    currentTick = lift currentTick
    {-# INLINE currentTick #-}

instance ( MonadMempool    tx   m
         , MonadBlockStore tx s m
         , Hashable        tx
         ) => MonadProtocol tx (NakamotoT tx s m)
  where
    stepM _ = \case
        P2P.BlockMsg blk ->
            isNovelBlock (blockHash blk) >>= \case
                True | Right blk' <- validateBlock blk -> do
                    NakamotoEnv{nakEval} <- ask
                    BlockStore.storeBlock $ toOrphan nakEval blk'
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
        NakamotoEnv{..} <- ask
        r :: StdGen     <- state split
        txs             <- map snd <$> getTxs
        parent          <- tip <$> BlockStore.maximumChainBy (comparing height)

        case nakMiner t r nakEval nakDifficulty txs parent of
            Just blk -> do
                delTxs (blockData blk)
                BlockStore.storeBlock $ map (const . Just) blk
                pure [P2P.BlockMsg (second (const ()) blk)]
            Nothing ->
                pure mempty

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

runNakamotoT :: NakamotoEnv tx s -> StdGen -> NakamotoT tx s m a -> m (a, StdGen, ())
runNakamotoT env rng (NakamotoT ma) = runRWST ma env rng

evalNakamotoT :: Monad m => NakamotoEnv tx s -> StdGen -> NakamotoT tx s m a -> m (a, ())
evalNakamotoT env rng (NakamotoT ma) = evalRWST ma env rng

-- | Attempt to mine a 'Block'. Returns 'Nothing' if all nonces were tried
-- unsuccessfully.
mineBlock :: Binary tx => NakamotoMiner tx s
mineBlock tick _ evalFn d txs parent = do
    let (results, s') = applyValidExprs txs (blockState . blockHeader $ parent) evalFn
        validTxs      = map fst (rights results)

    findBlock tick (blockHash parent) s' d validTxs

-- | Like 'mineBlock', buy attempts to mine a block only 10% of the time it
-- is called. Useful for test environments with very low difficulty.
mineBlockRandom :: Binary tx => NakamotoMiner tx s
mineBlockRandom t rng evalFn d txs parent =
    let (r, rng') = randomR (0, 1) rng
        p = 0.1 :: Float
     in if r < p
           then mineBlock t rng' evalFn d txs parent
           else Nothing

-- | Calculate block difficulty.
difficulty :: BlockHeader s -> Difficulty
difficulty = os2ip . headerHash

-- | The minimum difficulty.
minDifficulty :: Difficulty
minDifficulty =
    0xEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | An easy difficulty. About 24s per block on a single core.
easyDifficulty :: Difficulty
easyDifficulty =
    0x00000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

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
