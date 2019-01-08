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

import           Oscoin.Crypto.Blockchain

import           GHC.Exts (IsList(fromList))

-- | Validate a 'Blockchain' with the given validation function. Returns 'Right'
-- when valid.
validateBlockchain :: Validate tx s -> Blockchain tx s -> Either ValidationError ()
validateBlockchain validateBlock (Blockchain (blk :| [])) =
    validateBlock [] blk
validateBlockchain validateBlock (Blockchain (blk :| blks)) =
    validateBlock blks blk *> validateBlockchain validateBlock (fromList blks)
