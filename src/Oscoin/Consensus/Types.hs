module Oscoin.Consensus.Types
    ( ChainScore
    , Consensus(..)
    , Miner
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain

data Consensus tx s m = Consensus
    { cScore :: ChainScore tx s
    , cMiner :: Miner s m
    }

type ChainScore tx s = Blockchain tx s -> Blockchain tx s -> Ordering

type Miner s m = forall tx unsealed.
       Maybe (Blockchain tx s)
    -> BlockHeader unsealed
    -> m (Maybe (BlockHeader s))
