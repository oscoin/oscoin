-- | In simple consensus the set of nodes is fixed and each node has a
-- position in the set of nodes. The nodes take turns mining blocks
-- according to their position.
module Oscoin.Consensus.Simple
    ( simpleConsensus

    , MonadLastTime(..)
    , mineSimple
    , reconcileSimple

    , epochLength
    , chainScore
    , shouldCutBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (Tick)
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..))
import           Oscoin.Consensus.Types (Consensus(..), Miner)
import           Oscoin.Crypto.Blockchain (Blockchain, height, tip)
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHash, BlockHeader(..))


-- | The position of a node within a set of participating nodes.
-- @(k, n)@ means that the node has index `k` in a set of `n` nodes.
type Position = (Int, Int)

class Monad m => MonadLastTime m where
    getLastBlockTick :: m Tick
    setLastBlockTick :: Tick -> m ()

    getLastAskTick   :: m Tick
    setLastAskTick   :: Tick -> m ()

epochLength :: Tick
epochLength = 1

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
    let blockHeaderTick = fromInteger $ toInteger blockTimestamp
    if shouldCutBlock position lastBlk blockHeaderTick
    then do
        setLastBlockTick blockHeaderTick
        pure $ Just bh
    else
        pure Nothing

reconcileSimple
    :: ( MonadBlockStore tx () m
       , MonadLastTime         m
       )
    => Tick
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
    h              = height bc
    lastBlock      = tip bc
    timestampNs    = blockTimestamp $ blockHeader lastBlock
    timestamp      = timestampNs `div` 1000000000000
    e              = round epochLength
    steps          = fromIntegral timestamp `div` e :: Int
    bigMagicNumber = 2526041640 -- some loser in 2050 has to deal with this bug

shouldReconcile :: Tick -> Tick -> Bool
shouldReconcile lastAsk at = time - round lastAsk >= stepTime
  where
    time     = round at :: Int
    stepTime = round epochLength

shouldCutBlock :: Position -> Tick -> Tick -> Bool
shouldCutBlock (outOffset, total) lastBlk at = beenAWhile && ourTurn
  where
    time              = round at
    stepTime          = round epochLength
    relativeBlockTime = stepTime * total
    beenAWhile        = time - round lastBlk >= relativeBlockTime
    stepNumber        = time `div` stepTime
    currentOffset     = stepNumber `mod` total
    ourTurn           = currentOffset == outOffset
