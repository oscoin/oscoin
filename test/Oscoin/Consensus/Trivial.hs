module Oscoin.Consensus.Trivial
    ( trivialConsensus
    ) where


import           Oscoin.Prelude

import           Oscoin.Consensus.Types (Consensus(..))
import           Oscoin.Crypto.Blockchain (Blockchain, height)
import           Oscoin.Crypto.Blockchain.Block
                 (Block, Depth, Sealed, Unsealed, sealBlock)
import           Oscoin.Crypto.Hash (HasHashing, Hash)

import           Codec.Serialise (Serialise)

-- | Trivial consensus always mines a block immediately and uses chain height as
-- its score.
trivialConsensus
    :: ( Monad m
       , Serialise s
       , Serialise (Hash c)
       , HasHashing c
       )
    => s
    -> Consensus c tx s m
trivialConsensus seal = Consensus
    { cScore = comparing chainScore
    , cMiner = mineBlock seal
    , cValidate = \_ _ -> Right ()
    }

chainScore :: Blockchain c tx s -> Int
chainScore = fromIntegral . height

mineBlock
    :: ( Serialise s
       , Serialise (Hash c)
       , Applicative m
       , HasHashing c
       )
    => s
    -> (Depth -> m [Block c tx (Sealed c s)])
    -> Block c tx Unsealed
    -> m (Maybe (Block c tx (Sealed c s)))
mineBlock seal _ blk =
    pure . Just . sealBlock seal $ blk
