module Oscoin.P2P.Discovery
    ( Disco(knownPeers, closeDisco)

    , withDisco
    ) where

import Oscoin.P2P.Discovery.Internal (Disco(closeDisco, knownPeers))

import Control.Exception.Safe (bracket, MonadMask)

withDisco :: MonadMask m => m (Disco m) -> (Disco m -> m a) -> m a
withDisco mkDisco = bracket mkDisco closeDisco
