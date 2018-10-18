module Oscoin.Consensus.Trivial
    ( trivialConsensus
    ) where


import           Oscoin.Prelude

import           Oscoin.Consensus.Types (Consensus(..))
import           Oscoin.Crypto.Blockchain (Blockchain, height)
import           Oscoin.Crypto.Blockchain.Block (BlockHeader)

-- | Trivial consensus always mines a block immediately and uses chain height as
-- its score.
trivialConsensus :: (Monad m) => Consensus tx m
trivialConsensus = Consensus
    { cScore = comparing chainScore
    , cMiner = mineBlock
    }

chainScore :: Blockchain tx s -> Int
chainScore = fromIntegral . height

mineBlock :: Applicative m => Maybe (Blockchain tx s) -> BlockHeader a -> m (Maybe (BlockHeader a))
mineBlock _chain bh = pure $ Just bh
