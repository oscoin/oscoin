module Oscoin.Protocol.Internal
    ( Protocol(..)
    ) where

import           Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Types (Validate)
import           Oscoin.Crypto.Blockchain.Block hiding (parentHash)
import           Oscoin.Storage.Block.BlockTree

-- | This data structure incorporates all the different components and
-- data structures in the system devoted to store \"things\" and enforce
-- the Oscoin protocol, which entails also chain selection and blocks
-- validation. Certain components (like the block cache, for example) are not
-- managed by the 'Protocol' directly but they are rather an implementation
-- detail of whichever 'BlockStore' this structure encapsulates.
data Protocol c tx s m = Protocol
    { protoBlockTree    :: BlockTree c tx s m
    -- ^ The 'BlockTree', an opaque interface to a component which only
    -- responsibility is to store and retrieve blocks.
    , protoValidateFull :: Validate c tx s
    -- ^ A validation function to (fully) validate a block.
    , protoScoreBlock   :: Block c tx (Sealed c s) -> Score
    -- ^ A function to score blocks.
    , protoConfig       :: Consensus.Config
    }
