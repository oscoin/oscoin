module Oscoin.Consensus.Types
    ( ChainScore
    , Consensus(..)
    , Miner
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain

data Consensus tx m = Consensus
    { cScore :: ChainScore tx
    , cMiner :: Miner m
    }

type ChainScore tx = forall s. Blockchain tx s -> Blockchain tx s -> Ordering
type Miner m       = forall a. BlockHeader a -> m (Maybe (BlockHeader a))
