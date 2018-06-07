-- | In-memory 'Disco' suitable for testing in concurrent settings
module Oscoin.P2P.Discovery.InMemory
    ( Peers
    , mkPeers

    , mkDisco

    , addPeer
    , removePeer
    ) where

import           Oscoin.P2P.Discovery.Internal (Disco(..))
import           Oscoin.Prelude

import           Control.Concurrent.STM.TVar
import qualified Data.Set as Set


newtype Peers addr = Peers (TVar (Set addr))

mkPeers :: Set addr -> IO (Peers addr)
mkPeers = map Peers . newTVarIO

mkDisco :: Peers addr -> Disco IO addr
mkDisco (Peers peers) = Disco
    { knownPeers = readTVarIO peers
    , closeDisco = pure ()
    }

addPeer :: Ord addr => Peers addr -> addr -> IO ()
addPeer (Peers peers) addr = atomically $
    modifyTVar' peers (Set.insert addr)

removePeer :: Ord addr => Peers addr -> addr -> IO ()
removePeer (Peers peers) addr = atomically $
    modifyTVar' peers (Set.delete addr)
