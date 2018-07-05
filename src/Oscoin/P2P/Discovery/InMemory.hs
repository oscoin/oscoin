-- | In-memory 'Disco' suitable for testing in concurrent settings
module Oscoin.P2P.Discovery.InMemory
    ( Peers
    , mkPeers

    , mkDisco

    , addPeer
    , removePeer
    ) where

import           Oscoin.P2P.Discovery.Internal (Disco(..))
import           Oscoin.P2P.Types (Endpoints, NodeId)
import           Oscoin.Prelude

import           Control.Concurrent.STM.TVar
import qualified Data.Map as Map


newtype Peers = Peers (TVar (Map NodeId Endpoints))

mkPeers :: Map NodeId Endpoints -> IO Peers
mkPeers = map Peers . newTVarIO

mkDisco :: Peers -> Disco IO
mkDisco (Peers peers) = Disco
    { knownPeers = readTVarIO peers
    , closeDisco = pure ()
    }

addPeer :: Peers -> NodeId -> Endpoints -> IO ()
addPeer (Peers peers) id addr = atomically $
    modifyTVar' peers (Map.insert id addr)

removePeer :: Peers -> NodeId -> IO ()
removePeer (Peers peers) id = atomically $
    modifyTVar' peers (Map.delete id)
