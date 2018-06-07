-- | Trivial 'Disco' based on the state monad.
module Oscoin.P2P.Discovery.Pure
    ( mkDisco
    , addPeer
    , removePeer
    ) where

import           Oscoin.P2P.Discovery.Internal (Disco(..))
import           Oscoin.Prelude

import           Control.Monad.State (MonadState, get, modify')
import qualified Data.Set as Set

mkDisco :: MonadState (Set addr) m => Disco m addr
mkDisco = Disco
    { knownPeers = get
    , closeDisco = pure ()
    }

addPeer :: (Ord addr, MonadState (Set addr) m) => addr -> m ()
addPeer addr = modify' (Set.insert addr)

removePeer :: (Ord addr, MonadState (Set addr) m) => addr -> m ()
removePeer addr = modify' (Set.delete addr)
