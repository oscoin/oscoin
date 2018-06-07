module Oscoin.P2P.Discovery.Internal (Disco (..)) where

import Oscoin.Prelude

-- | Peer discovery interface
data Disco m addr = Disco
    { knownPeers :: m (Set addr)
    -- ^ Return the set of currently known peer addresses.
    --
    -- Implementations are expected to update this asynchronously, such that
    -- calling 'knownPeers' is a relatively cheap operation.
    , closeDisco :: m ()
    }
