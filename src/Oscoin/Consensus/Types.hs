module Oscoin.Consensus.Types
    ( Consensus(..)
    , ChainScore
    , Miner
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain

data Consensus tx s m = Consensus
    { cScore :: ChainScore tx s
    , cMiner :: Miner m
    }

type ChainScore tx s = Blockchain tx s -> Blockchain tx s -> Ordering

type Miner m = forall a. BlockHeader a -> m (Maybe (BlockHeader a))
