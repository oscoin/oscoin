module Oscoin.Node.Channel where

import Oscoin.Prelude
import Control.Concurrent.STM.TQueue (TQueue, newTQueue)

data Event tx = Event (Id tx) tx

type Channel tx = TQueue (Event tx)

newChannel :: MonadSTM m => m (Channel tx)
newChannel = liftSTM newTQueue

newtype Subscription = Subscription Text
    deriving (Eq, Ord, Show)

type instance Id (Channel tx) = Subscription
