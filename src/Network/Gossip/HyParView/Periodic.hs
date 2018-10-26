module Network.Gossip.HyParView.Periodic
    ( Config
    , defaultConfig
    , withPeriodic
    ) where

import           Prelude

import qualified Network.Gossip.HyParView as H

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
                 (Async, Concurrently(..), async, uninterruptibleCancel)
import           Control.Exception.Safe (bracket)
import           Control.Monad (forever)
import           Data.Hashable (Hashable)
import           Data.Time.Clock (NominalDiffTime)

{-# ANN module ("HLint: ignore Use map" :: String) #-}

data Config = Config
    { confShuffleInterval         :: NominalDiffTime
    , confRandomPromotionInterval :: NominalDiffTime
    }

defaultConfig :: Config
defaultConfig = Config
    { confShuffleInterval         = 10
    , confRandomPromotionInterval = 5
    }

newtype Handle = Handle (Async ())

new :: (Eq n, Hashable n)
    => Config
    -> (H.HyParView n () -> IO ())
    -> IO Handle
new Config { confShuffleInterval, confRandomPromotionInterval } run =
    fmap Handle . async . runConcurrently $
        Concurrently shuffle <> Concurrently promote
  where
    shuffle = forever $ sleep confShuffleInterval         *> run H.shuffle
    promote = forever $ sleep confRandomPromotionInterval *> run H.promoteRandom

destroy :: Handle -> IO ()
destroy (Handle t) = uninterruptibleCancel t

withPeriodic
    :: (Eq n, Hashable n)
    => Config
    -> (H.HyParView n () -> IO ())
    -> (Handle -> IO a)
    -> IO a
withPeriodic cfg run = bracket (new cfg run) destroy

--------------------------------------------------------------------------------

sleep :: NominalDiffTime -> IO ()
sleep t = threadDelay $ toSeconds t * 1000000
  where
    toSeconds = round @Double . realToFrac
