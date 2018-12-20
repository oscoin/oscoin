module Oscoin.Consensus.Types
    ( ChainScore
    , Consensus(..)
    , Validate
    , Miner
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain

-- | Represents an abstract consensus protocol.
data Consensus tx s m = Consensus
    { cScore    :: ChainScore tx s
    , cMiner    :: Miner s m
    , cValidate :: Validate tx s
    }

-- | Block validation function.
type Validate tx s =
       [Block tx s]     -- ^ Ancestors of block to be validated.
    -> Block tx s       -- ^ Block to be validated.
    -> Either Text ()   -- ^ Either an error or @()@.

-- | Chain scoring function.
type ChainScore tx s = Blockchain tx s -> Blockchain tx s -> Ordering

-- | Block mining function.
type Miner s m = forall tx unsealed.
    (Depth -> m [Block tx s])
    -> BlockHeader unsealed
    -> m (Maybe (BlockHeader s))
