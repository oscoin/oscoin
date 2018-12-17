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
import           Oscoin.Crypto.Blockchain.Block (Difficulty(..))
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Crypto.Number.Serialize (os2ip)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.List.NonEmpty as NonEmpty

blockTime :: Duration
blockTime = 1 * seconds

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

nakamotoConsensus :: (Monad m) => Consensus tx PoW m
nakamotoConsensus = Consensus
    { cScore = comparing chainScore
    , cMiner = mineNakamoto chainDifficulty
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
difficulty = Difficulty . os2ip . headerHash

-- | Number of blocks to consider for difficulty calculation. Roughly 2 weeks
-- withh a 10 min block time.
difficultyBlocks :: Depth
difficultyBlocks = 2016

-- | Calculate the difficulty of a blockchain.
chainDifficulty :: [Block tx PoW] -> Difficulty
chainDifficulty [] =
    minGenesisDifficulty
chainDifficulty (NonEmpty.fromList -> blks)
    | NonEmpty.length blks `mod` fromIntegral difficultyBlocks == 0 =
        Difficulty $ fromDifficulty currentDifficulty
                   * fromIntegral targetElapsed
                   `div` toInteger actualElapsed
    | otherwise =
        prevDifficulty
  where
    blocksConsidered  = fromIntegral difficultyBlocks
    prevDifficulty    = blockDifficulty . blockHeader $ NonEmpty.head blks

    range             = NonEmpty.fromList $ NonEmpty.take blocksConsidered blks

    rangeStart        = blockHeader $ NonEmpty.last range
    rangeEnd          = blockHeader $ NonEmpty.head range

    actualElapsed     = blockTimestamp rangeEnd `timeDiff` blockTimestamp rangeStart
    targetElapsed     = fromIntegral blocksConsidered * blockTime

    currentDifficulty = blockDifficulty rangeEnd
