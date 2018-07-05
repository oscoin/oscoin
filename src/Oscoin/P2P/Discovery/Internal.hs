module Oscoin.P2P.Discovery.Internal (Disco (..)) where

import Oscoin.Prelude
import Oscoin.P2P.Types

-- | Peer discovery interface
data Disco m = Disco
    { knownPeers :: m (Map NodeId Endpoints)
    -- ^ Return the set of currently known peer addresses.
    --
    -- Implementations are expected to update this asynchronously, such that
    -- calling 'knownPeers' is a relatively cheap operation.
    , closeDisco :: m ()
    }

instance Applicative m => Semigroup (Disco m) where
    a <> b = Disco
        { knownPeers = liftA2 (<>) (knownPeers a) (knownPeers b)
        , closeDisco = closeDisco a *> closeDisco b
        }

instance Applicative m => Monoid (Disco m) where
    mempty  = Disco { knownPeers = pure mempty, closeDisco = pure () }
    mappend = (<>)
