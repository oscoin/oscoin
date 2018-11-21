module Oscoin.Consensus.Nakamoto
    ( nakamotoConsensus
    , mineNakamoto
    , minDifficulty
    , easyDifficulty
    , defaultGenesisDifficulty
    , difficulty
    , PoW(..)
    , emptyPoW
    , hasPoW
    , chainDifficulty
    , chainScore
    , blockTime
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Crypto.Number.Serialize (os2ip)
import           Data.Aeson (FromJSON, ToJSON)
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

minGenesisDifficulty :: Difficulty
minGenesisDifficulty = minDifficulty

-- | A PoW nonce.
type Nonce = Word32

-- | A PoW seal.
newtype PoW = PoW Nonce
    deriving (Eq, Ord, Show, Generic)

instance ToJSON PoW
instance FromJSON PoW

-- | An empty (zero nonce) proof-of-work.
emptyPoW :: PoW
emptyPoW = PoW 0

instance Serialise PoW

nakamotoConsensus :: (Monad m) => Maybe Difficulty -> Consensus tx PoW m
nakamotoConsensus difi = Consensus
    { cScore = comparing chainScore
    , cMiner = mineNakamoto $ maybe chainDifficulty const difi
    }

mineNakamoto
    :: forall m. Monad m
    => (forall tx. [Block tx PoW] -> Difficulty)
    -> Miner PoW m
mineNakamoto difiFn getBlocks bh = do
    blks <- getBlocks difficultyBlocks
    go $ bh { blockDifficulty = difiFn blks } $> PoW 0
  where
    go :: BlockHeader PoW -> m (Maybe (BlockHeader PoW))
    go hdr@BlockHeader { blockSeal = PoW nonce }
        | hasPoW hdr            = pure $ Just $ hdr $> PoW nonce
        | nonce < maxBound      = go hdr { blockSeal = PoW (nonce + 1) }
        | otherwise             = pure Nothing

chainScore :: Blockchain tx PoW -> Int
chainScore = fromIntegral . height

hasPoW :: BlockHeader PoW -> Bool
hasPoW header =
    difficulty header < blockDifficulty header

difficulty :: BlockHeader PoW -> Difficulty
difficulty = os2ip . headerHash

-- | Number of blocks to consider in Nakamoto consensus. Roughly 2 weeks.
difficultyBlocks :: Depth
difficultyBlocks = 2016

-- | Calculate the difficulty of a blockchain.
chainDifficulty :: [Block tx PoW] -> Difficulty
chainDifficulty [] =
    minGenesisDifficulty
chainDifficulty (NonEmpty.fromList -> blks) =
    computedDifficulty . NonEmpty.fromList $ NonEmpty.take blocksConsidered blks
  where
    blocksConsidered  = timePeriodMinutes `div` blockTimeMinutes
    timePeriodMinutes = timePeriodDays * 24 * 60
    timePeriodDays    = 2 * 7 -- Two weeks.
    blockTimeMinutes  = 10
    blockTimeSeconds  = blockTimeMinutes * 60
    oldDifficulty     = blockDifficulty . blockHeader $ NonEmpty.last blks

    computedDifficulty range
        | NonEmpty.length range < blocksConsidered = oldDifficulty
        | otherwise =
        let rangeStart        = blockHeader . NonEmpty.last
                              $ NonEmpty.head blks :| NonEmpty.tail range
            rangeEnd          = blockHeader $ NonEmpty.head blks
            actualElapsed     = blockTimestamp rangeEnd `timeDiff` blockTimestamp rangeStart
            targetElapsed     = fromIntegral $ blocksConsidered * blockTimeSeconds
            currentDifficulty = blockDifficulty rangeEnd
         in currentDifficulty * targetElapsed `div` toInteger actualElapsed
