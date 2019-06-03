module Oscoin.Telemetry.Events.Sync
    ( NodeSyncEvent(..)
    ) where

import           Oscoin.Crypto.Blockchain.Block (BlockHash, Height)
import           Oscoin.Prelude

data NodeSyncEvent c =
      NodeSyncStarted (BlockHash c, Height) (BlockHash c, Height)
      -- ^ The syncing process has started, with
      -- the first argument being the node's local
      -- tip and the second the remote tip.
    | NodeSyncFinished (BlockHash c, Height)
    | NodeSyncFetched Int
      -- ^ The given number of blocks was fetched directly from the requested
      -- peer.
    | NodeSyncMissing Int
      -- ^ The given number of blocks needed to be fetched from a different
      -- peer.
    | NodeSyncError SomeException
      -- ^ Temporary loose representation of errors.

