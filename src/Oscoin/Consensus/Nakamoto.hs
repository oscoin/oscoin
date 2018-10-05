module Oscoin.Consensus.Nakamoto
    ( nakamotoConsensus
    , mineNakamoto
    , minDifficulty
    , easyDifficulty
    , defaultGenesisDifficulty
    , difficulty
    , hasPoW
    , chainDifficulty
    , chainScore
    , blockTime
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import           Oscoin.Time

import           Crypto.Number.Serialize (os2ip)
import qualified Data.List.NonEmpty as NonEmpty

blockTime :: Duration
blockTime = 1 * seconds

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

nakamotoConsensus :: (Applicative m) => Maybe Difficulty -> Consensus tx m
nakamotoConsensus difi = Consensus
    { cScore = comparing chainScore
    , cMiner = mineNakamoto $ maybe chainDifficulty const difi
    }

mineNakamoto
    :: Applicative m
    => (forall tx s. Blockchain tx s -> Difficulty)
    -> Miner m
mineNakamoto difi chain bh = go bh { blockDifficulty = difi chain }
  where
    go hdr@BlockHeader { blockNonce }
        | hasPoW hdr            = pure $ Just hdr
        | blockNonce < maxBound = go hdr { blockNonce = blockNonce + 1 }
        | otherwise             = pure Nothing

chainScore :: Blockchain tx s -> Int
chainScore = fromIntegral . height

hasPoW :: BlockHeader s -> Bool
hasPoW header =
    difficulty header < blockDifficulty header

difficulty :: BlockHeader s -> Difficulty
difficulty = os2ip . headerHash

-- | Calculate the difficulty of a blockchain.
chainDifficulty :: Blockchain tx s -> Difficulty
chainDifficulty (Blockchain blks) =
    let range = nonEmpty $ NonEmpty.take blocksConsidered blks
     in maybe genesisDifficulty computedDifficulty range
  where
    blocksConsidered  = timePeriodMinutes `div` blockTimeMinutes
    timePeriodMinutes = timePeriodDays * 24 * 60
    timePeriodDays    = 2 * 7 -- Two weeks.
    blockTimeMinutes  = 10
    blockTimeSeconds  = blockTimeMinutes * 60

    genesisDifficulty = blockDifficulty . blockHeader $ NonEmpty.last blks

    computedDifficulty range
        | NonEmpty.length range < blocksConsidered = genesisDifficulty
        | otherwise =
        let rangeStart        = blockHeader . NonEmpty.last
                              $ NonEmpty.head blks :| NonEmpty.tail range
            rangeEnd          = blockHeader $ NonEmpty.head blks
            actualElapsed     = blockTimestamp rangeEnd `timeDiff` blockTimestamp rangeStart
            targetElapsed     = fromIntegral $ blocksConsidered * blockTimeSeconds
            currentDifficulty = blockDifficulty rangeEnd
         in currentDifficulty * targetElapsed `div` toInteger actualElapsed
