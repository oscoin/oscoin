-- | In simple consensus the set of nodes is fixed and each node has a
-- position in the set of nodes. The nodes take turns mining blocks
-- according to their position.
module Oscoin.Consensus.Simple
    ( simpleConsensus

    , PoA
    , MonadLastTime(..)
    , mineSimple

    , blockTime
    , chainScore
    , shouldCutBlock
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto

import           Oscoin.Consensus.Types (Consensus(..), Miner)
import           Oscoin.Crypto.Blockchain (Blockchain, height, tip)
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHeader(..), Sealed(..), sealBlock)
import           Oscoin.Time


-- | The position of a node within a set of participating nodes.
-- @(k, n)@ means that the node has index `k` in a set of `n` nodes.
type Position = (Int, Int)

-- | The simple consensus seal (proof-of-authority). Currently just a placeholder.
type PoA = ()

class Monad m => MonadLastTime m where
    getLastBlockTick :: m Timestamp
    setLastBlockTick :: Timestamp -> m ()

    getLastAskTick   :: m Timestamp
    setLastAskTick   :: Timestamp -> m ()

blockTime :: Duration
blockTime = 1 * seconds

simpleConsensus
    :: ( MonadLastTime m
       , Crypto.Hashable c (BlockHeader c (Sealed c PoA))
       )
    => Position
    -> Consensus c tx PoA m
simpleConsensus position = Consensus
    { cScore = comparing chainScore
    , cMiner = mineSimple position
    , cValidate = \_ _ -> Right ()
    }

mineSimple
    :: ( MonadLastTime m
       , Crypto.Hashable c (BlockHeader c (Sealed c PoA))
       )
    => Position
    -> Miner c PoA m
mineSimple position _chain unsealedBlock = do
    let bh = blockHeader unsealedBlock
    lastBlk <- getLastBlockTick
    if shouldCutBlock position lastBlk (blockTimestamp bh)
    then do
        setLastBlockTick (blockTimestamp bh)
        pure $ Just $ sealBlock () unsealedBlock
    else
        pure Nothing

chainScore :: Blockchain c tx s -> Int
chainScore bc =
    (bigMagicNumber * h) - steps
  where
    h              = fromIntegral $ height bc
    lastBlock      = tip bc
    timestamp      = blockTimestamp $ blockHeader lastBlock
    steps          = fromIntegral $ sinceEpoch timestamp `div` blockTime
    bigMagicNumber = 2526041640 -- some loser in 2050 has to deal with this bug

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
