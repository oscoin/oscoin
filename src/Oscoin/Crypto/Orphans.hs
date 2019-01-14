{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Crypto.Orphans where


import           Oscoin.Prelude

import           Crypto.Random.Types (MonadRandom(..))

instance MonadRandom m => MonadRandom (ReaderT e m) where
    getRandomBytes = lift . getRandomBytes


