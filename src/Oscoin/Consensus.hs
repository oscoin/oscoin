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
import           Oscoin.ProtocolConfig (ProtocolConfig)

import           GHC.Exts (IsList(fromList))

-- | Validate a 'Blockchain' with the given validation function. Returns 'Right'
-- when valid.
validateBlockchain :: ProtocolConfig
                   -> Validate tx s
                   -> Blockchain tx s
                   -> Either ValidationError ()
validateBlockchain protocolConfig validateBlock (Blockchain (blk :| [])) =
    validateBlock protocolConfig [] blk
validateBlockchain protocolConfig validateBlock (Blockchain (blk :| blks)) =
    validateBlock protocolConfig blks blk *>
    validateBlockchain protocolConfig validateBlock (fromList blks)
