module Oscoin.Storage.Block.BlockTree
    ( BlockTree(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block hiding (parentHash)
import           Oscoin.Storage.Block.Abstract (BlockStore)

-- | A 'BlockTree' is conceputally a rose tree where forests of orphans might
-- be fused together as more blocks arrives and each orphan chain grows.
data BlockTree c tx s m = BlockTree
    { btFullBlockStore :: BlockStore c tx s m
    -- ^ An opaque 'BlockStore'.
    , insertOrphan     :: Block c tx (Sealed c s) -> m (BlockTree c tx s m)
    -- | Returns 'True' if the input block is novel to the 'BlockTree', i.e it's
    -- neither in the orphan store nor in the block store.
    , member           :: BlockHash c -> m Bool
    }
