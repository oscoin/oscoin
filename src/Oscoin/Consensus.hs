module Oscoin.Consensus
    ( nakamotoConsensus
    , simpleConsensus
    , validateBlockchain
    , module Oscoin.Consensus.Mining
    , module Oscoin.Consensus.Types
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Mining
import           Oscoin.Consensus.Nakamoto (nakamotoConsensus)
import           Oscoin.Consensus.Simple (simpleConsensus)
import           Oscoin.Consensus.Types

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain

import           GHC.Exts (IsList(fromList))

-- | Validate a 'Blockchain' with the given validation function. Returns 'Right'
-- when valid.
validateBlockchain :: Consensus.Config
                   -> Validate tx s
                   -> Blockchain tx s
                   -> Either ValidationError ()
validateBlockchain config validateBlock (Blockchain (blk :| [])) =
    validateBlock config [] blk
validateBlockchain config validateBlock (Blockchain (blk :| blks)) =
    validateBlock config blks blk *>
    validateBlockchain config validateBlock (fromList blks)
