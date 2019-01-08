module Oscoin.Consensus.Types
    ( ChainScore
    , Consensus(..)
    , Validate
    , ValidationError(..)
    , Miner
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Time (Duration)

-- | Represents an abstract consensus protocol.
data Consensus tx s m = Consensus
    { cScore    :: ChainScore tx s
    , cMiner    :: Miner s m
    , cValidate :: Validate tx s
    }

-- | Block validation function.
type Validate tx s =
       [Block tx s]                -- ^ Ancestors of block to be validated.
    -> Block tx s                  -- ^ Block to be validated.
    -> Either ValidationError ()   -- ^ Either an error or @()@.

data ValidationError =
      InvalidParentHash       Crypto.Hash
    -- ^ Parent block hash doesn't match
    | InvalidDataHash         Crypto.Hash
    -- ^ Block data hash doesn't match data
    | InvalidTargetDifficulty TargetDifficulty TargetDifficulty
    -- ^ Expected and actual target difficulty
    | InvalidBlockDifficulty  Difficulty       TargetDifficulty
    -- ^ Block difficulty doesn't match target
    | InvalidBlockTimestamp   Duration
    -- ^ Negative duration means the block is in the past, positive means too
    -- far in the future
    deriving (Eq, Show)

-- | Chain scoring function.
type ChainScore tx s = Blockchain tx s -> Blockchain tx s -> Ordering

-- | Block mining function.
type Miner s m = forall tx unsealed.
    (Depth -> m [Block tx s])
    -> BlockHeader unsealed
    -> m (Maybe (BlockHeader s))
