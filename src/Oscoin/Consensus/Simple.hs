-- | In simple consensus the set of nodes is fixed and each node has a
-- position in the set of nodes. The nodes take turns mining blocks
-- according to their position.
module Oscoin.Consensus.Simple
    ( simpleConsensus

    , MonadLastTime(..)
    , mineSimple
    , reconcileSimple

    , blockTime
    , chainScore
    , shouldCutBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types (Consensus(..), Miner)
import           Oscoin.Crypto.Blockchain (Blockchain, height, tip)
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHash, BlockHeader(..))
import           Oscoin.Storage.Block.Class (MonadBlockStore(..))
import           Oscoin.Time


-- | The position of a node within a set of participating nodes.
-- @(k, n)@ means that the node has index `k` in a set of `n` nodes.
type Position = (Int, Int)

class Monad m => MonadLastTime m where
    getLastBlockTick :: m Timestamp
    setLastBlockTick :: Timestamp -> m ()

    getLastAskTick   :: m Timestamp
    setLastAskTick   :: Timestamp -> m ()

blockTime :: Duration
blockTime = 1 * seconds

simpleConsensus
    :: (MonadLastTime m)
    => Position
    -> Consensus tx m
simpleConsensus position = Consensus
    { cScore = comparing chainScore
    , cMiner = mineSimple position
    }

mineSimple
    :: (MonadLastTime m)
    => Position
    -> Miner m
mineSimple position _chain bh@BlockHeader{blockTimestamp} = do
    lastBlk <- getLastBlockTick
    -- FIXME(kim): this seems wrong
    if shouldCutBlock position lastBlk blockTimestamp
    then do
        setLastBlockTick blockTimestamp
        pure $ Just bh
    else
        pure Nothing

reconcileSimple
    :: ( MonadBlockStore tx () m
       , MonadLastTime         m
       )
    => Timestamp
    -> m [BlockHash]
reconcileSimple tick = do
    lastAsk <- getLastAskTick
    if shouldReconcile lastAsk tick
    then do
        setLastAskTick tick
        toList <$> orphans
    else
        pure []

chainScore :: Blockchain tx s -> Int
chainScore bc =
    (bigMagicNumber * h) - steps
  where
    h              = fromIntegral $ height bc
    lastBlock      = tip bc
    timestamp      = blockTimestamp $ blockHeader lastBlock
    steps          = fromIntegral $ sinceEpoch timestamp `div` blockTime
    bigMagicNumber = 2526041640 -- some loser in 2050 has to deal with this bug

shouldReconcile :: Timestamp -> Timestamp -> Bool
shouldReconcile lastAsk at = at `timeDiff` lastAsk >= blockTime

shouldCutBlock :: Position -> Timestamp -> Timestamp -> Bool
shouldCutBlock (outOffset, total) lastBlk at = beenAWhile && ourTurn
  where
    time              = at
    relativeBlockTime = blockTime * total'
    beenAWhile        = time `timeDiff` lastBlk >= relativeBlockTime
    stepNumber        = sinceEpoch time `div` blockTime
    currentOffset     = stepNumber `mod` total'
    ourTurn           = currentOffset == fromIntegral outOffset
    total'            = fromIntegral total
