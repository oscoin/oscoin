-- | Trivial 'Disco' based on the state monad.
module Oscoin.P2P.Discovery.Pure
    ( mkDisco
    , addPeer
    , removePeer
    , toKnownPeers
    ) where

import           Oscoin.Prelude

import           Oscoin.P2P.Discovery.Internal (Disco(..))
import           Oscoin.P2P.Types (Endpoints, NodeId, Seed(..))

import           Control.Monad.State (MonadState, gets, modify')
import qualified Data.Map as Map
import           Lens.Micro (over)

type EndpointMap = Map NodeId Endpoints

mkDisco :: (Has EndpointMap s, MonadState s m) => Disco m
mkDisco = Disco
    { knownPeers = gets getter
    , closeDisco = pure ()
    }

addPeer :: (Has EndpointMap s, MonadState s m) => NodeId -> Endpoints -> m ()
addPeer id = modify' . over hasLens . Map.insert id

removePeer :: (Has EndpointMap s, MonadState s m) => NodeId -> m ()
removePeer = modify' . over (hasLens @EndpointMap) . Map.delete

toKnownPeers :: [Seed] -> EndpointMap
toKnownPeers ss =
    Map.fromList [(seedId, seedEndpoints) | Seed{..} <- ss]
