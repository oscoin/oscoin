{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Consensus.Nakamoto
    ( NakamotoT
    , NakamotoEnv(..)

    , defaultNakamotoEnv

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
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Evaluator
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash (Hashable, Hashed, hash, zeroHash)
import qualified Oscoin.Logging as Log
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Node.Mempool.Class as Mempool
import qualified Oscoin.P2P as P2P

import           Control.Monad.RWS (RWST, evalRWST, runRWST, state)
import           Crypto.Number.Serialize (os2ip)
import           Codec.Serialise (Serialise)
import           Data.Functor (($>))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList, catMaybes)
import           Data.Traversable (for)
import           System.Random (StdGen, split)

epochLength :: Tick
epochLength = 1

type NakamotoEval tx s = Evaluator s tx ()

-- | A Nakamoto mining function. Tries all nonces and returns 'Nothing' if
-- no block satisfying the difficulty was found.
type NakamotoMiner = forall a. StdGen -> BlockHeader a -> Maybe (BlockHeader a)

-- | Read-only environment for the Nakamoto consensus protocol.
data NakamotoEnv tx s = NakamotoEnv
    { nakEval       :: NakamotoEval tx s
    -- ^ Evaluation function to use for expressions
    , nakDifficulty :: Difficulty
    -- ^ Target difficulty (Nb. this is fixed for now and does not adjust)
    , nakMiner      :: NakamotoMiner
    -- ^ Mining function to use
    , nakLogger     :: Log.Logger
    }

instance Has Log.Logger (NakamotoEnv tx s) where
    getter         = nakLogger
    modifier f env = env { nakLogger = f (nakLogger env) }

defaultNakamotoEnv :: NakamotoEnv tx s
defaultNakamotoEnv = NakamotoEnv
    { nakEval = identityEval
    , nakDifficulty = easyDifficulty
    , nakMiner = mineBlock
    , nakLogger = Log.noLogger
    }

newtype NakamotoT tx s m a = NakamotoT (RWST (NakamotoEnv tx s) () StdGen m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader (NakamotoEnv tx s)
             , MonadWriter ()
             , MonadState StdGen
             , MonadTrans
             )

instance (Monad m, MonadClock m) => MonadClock (NakamotoT tx s m)

score :: Blockchain tx s -> Blockchain tx s -> Ordering
score = comparing height


instance ( MonadMempool    tx   m
         , MonadBlockStore tx s m
         , Hashable        tx
         , Serialise       tx
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
            pure . maybeToList . map (P2P.BlockMsg . void) $ mblk

    tickM t = do
        NakamotoEnv{..} <- ask
        txs             <- map snd <$> getTxs
        parent          <- tip <$> BlockStore.maximumChainBy (comparing height)
        let (results, newState) = applyValidExprs txs (blockState . blockHeader $ parent) nakEval
            validTxs = map fst (rights results)
            headerWithoutPoW = BlockHeader
                { blockPrevHash     = blockHash parent
                , blockDataHash     = hashTxs validTxs
                , blockState        = newState
                , blockStateHash    = zeroHash
                , blockDifficulty   = nakDifficulty
                , blockTimestamp    = fromIntegral $ fromEnum t
                , blockNonce        = 0
                }

        minerStdGen <- state split
        case nakMiner minerStdGen headerWithoutPoW of
            Just header -> do
                delTxs txs
                let blk = mkBlock header txs
                -- When mining a block, we've already computed the final state
                -- of that block, but when storing a block received from the
                -- network, we havent't, we just have `s -> Maybe s`.
                --
                -- It's nice to think of mined and network blocks uniformly,
                -- so we have the same interface for both with 'BlockStore.storeBlock',
                -- but as an optimization, with mined blocks we just have a
                -- `(const . Just)` with the `s` we have already computed.
                -- This way, the state is only computed once, even when the
                -- BlockStore attempts to recompute it when linking the block
                -- to its parent.
                BlockStore.storeBlock $ map (\s -> \_ -> Just s) blk
                pure [P2P.BlockMsg (void blk)]
            Nothing ->
                pure mempty

    {-# INLINE stepM #-}
    {-# INLINE tickM #-}

instance P2P.MonadNetwork tx   m => P2P.MonadNetwork tx   (NakamotoT tx s m)
instance MonadMempool     tx   m => MonadMempool     tx   (NakamotoT tx s m)
instance MonadBlockStore  tx s m => MonadBlockStore  tx s (NakamotoT tx s m)

isNovelBlock :: (MonadBlockStore tx s m) => BlockHash -> m Bool
isNovelBlock h =
    isNothing <$> BlockStore.lookupBlock h

isNovelTx :: (MonadBlockStore tx s m, MonadMempool tx m) => Hashed tx -> m Bool
isNovelTx h = do
    inBlockStore <- BlockStore.lookupTx h
    inMempool    <- Mempool.lookupTx h
    pure . isNothing $ inBlockStore <|> inMempool

runNakamotoT :: NakamotoEnv tx s -> StdGen -> NakamotoT tx s m a -> m (a, StdGen, ())
runNakamotoT env rng (NakamotoT ma) = runRWST ma env rng

evalNakamotoT :: Monad m => NakamotoEnv tx s -> StdGen -> NakamotoT tx s m a -> m (a, ())
evalNakamotoT env rng (NakamotoT ma) = evalRWST ma env rng

-- | Try different nonces until we find a block header that has a valid
-- proof of work. See 'hasPow'
mineBlock :: StdGen -> BlockHeader a -> Maybe (BlockHeader a)
mineBlock stdGen bh@BlockHeader { blockNonce }
    | hasPoW bh =
        Just bh
    | blockNonce < maxBound =
        mineBlock stdGen bh { blockNonce = blockNonce + 1 }
    | otherwise =
        Nothing

-- | Return whether or not a block header has a valid proof of work. A
-- block header has a valid proof of work if the 'blockNonce' is chosen
-- such that the block header hash is smaller than its
-- 'blockDifficulty'
hasPoW :: BlockHeader s -> Bool
hasPoW header =
    difficulty header < blockDifficulty header


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
