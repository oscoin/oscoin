module Oscoin.Consensus
    ( nakamotoConsensus
    , simpleConsensus
    , module Oscoin.Consensus.Mining
    , module Oscoin.Consensus.Types
    ) where

import           Oscoin.Consensus.Mining
import           Oscoin.Consensus.Nakamoto (nakamotoConsensus)
import           Oscoin.Consensus.Simple (simpleConsensus)
import           Oscoin.Consensus.Types
