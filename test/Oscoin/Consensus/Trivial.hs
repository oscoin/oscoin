module Oscoin.Consensus.Trivial
    ( trivialConsensus
    ) where


import           Oscoin.Prelude

import           Oscoin.Consensus.Types (Consensus(..))
import           Oscoin.Crypto.Blockchain (Blockchain, height)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHeader, Depth)

-- | Trivial consensus always mines a block immediately and uses chain height as
-- its score.
trivialConsensus :: (Monad m) => s -> Consensus tx s m
trivialConsensus seal = Consensus
    { cScore = comparing chainScore
    , cMiner = mineBlock seal
    , cValidate = \_ _ _ -> Right ()
    }

chainScore :: Blockchain tx s -> Int
chainScore = fromIntegral . height

mineBlock :: Applicative m => s -> (Depth -> m [Block tx s]) -> BlockHeader a -> m (Maybe (BlockHeader s))
mineBlock seal _ bh = pure . Just $ bh $> seal
