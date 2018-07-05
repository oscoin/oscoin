module Oscoin.Node.Mempool.Event
    ( Event (..)
    , Channel
    , drainChannel
    ) where

import           Oscoin.Prelude

import           Control.Concurrent.STM (STM, TChan, tryReadTChan)

data Event tx =
      Insert [tx]
    | Remove [tx]
    deriving (Eq, Show)

type Channel tx = TChan (Event tx)

-- | Read all available 'Event's from the 'Channel' into a list.
--
-- This is mainly useful for testing, when there are no writers.
drainChannel :: Channel tx -> STM [Event tx]
drainChannel chan = reverse <$> loop []
  where
    loop !acc = do
        e <- tryReadTChan chan
        case e of
            Just x  -> loop (x:acc)
            Nothing -> pure acc
