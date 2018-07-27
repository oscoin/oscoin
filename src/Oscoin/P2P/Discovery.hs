module Oscoin.P2P.Discovery
    ( Disco(knownPeers, closeDisco)

    , withDisco
    , toKnownPeers
    ) where

import           Oscoin.P2P.Discovery.Internal (Disco(closeDisco, knownPeers))
import           Oscoin.P2P.Types (EndpointMap, Seed(..))

import           Control.Exception.Safe (MonadMask, bracket)
import qualified Data.Map as Map

withDisco :: MonadMask m => m (Disco m) -> (Disco m -> m a) -> m a
withDisco mkDisco = bracket mkDisco closeDisco

toKnownPeers :: [Seed] -> EndpointMap
toKnownPeers ss =
    Map.fromList [(seedId, seedEndpoints) | Seed{..} <- ss]
