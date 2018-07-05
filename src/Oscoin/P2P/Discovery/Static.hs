module Oscoin.P2P.Discovery.Static (mkDisco) where

import           Oscoin.Prelude

import           Oscoin.P2P.Discovery.Internal (Disco(..))
import           Oscoin.P2P.Types (Endpoints, NodeId)

mkDisco :: Applicative m => Map NodeId Endpoints -> Disco m
mkDisco peers = Disco
    { knownPeers = pure peers
    , closeDisco = pure ()
    }
