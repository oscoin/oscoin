module Oscoin.Protocol.Trace
    ( ProtocolEvent (..)
    , NodeSyncEvent(..)
    )
where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block

data ProtocolEvent c =
      BlockExtendedTip (BlockHash c)
    -- ^ The block with hash 'BlockHash' extended the local chain.
    | BlockStoredAsOrphan (BlockHash c) Int
    -- ^ The block with hash 'BlockHash' has been stored as orphan, alongside
    -- the current size of the orphanage (including the block just added).
    | RollbackOccurred Depth (BlockHash c)
    -- ^ A rollback of depth 'Depth' occurred, setting the tip at 'BlockHash'.
    | PotentialNewChainFound Score
    -- ^ A new potential chain with tip 'BlockHash' is being evaluated for
    -- adoption.

data NodeSyncEvent c =
      NodeSyncStarted (BlockHash c, Height) (BlockHash c, Height)
      -- ^ The syncing process has started, with
      -- the first argument being the node's local
      -- tip and the second the remote tip.
    | NodeSyncFinished (BlockHash c, Height)
    | NodeSyncDispatched (BlockHash c)
      -- ^ The given block has been successfully dispatched to upstream
      -- consumers.
    | NodeSyncError SomeException
      -- ^ Temporary loose representation of errors.

