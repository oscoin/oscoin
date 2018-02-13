module Oscoin.Node.Channel where

import Oscoin.Prelude
import Control.Concurrent.STM.TQueue (TQueue, newTQueue)

data Event tx = Event (Id tx) tx

type Channel tx = TQueue (Event tx)

type instance Id (Channel tx) = Subscription

newChannel :: MonadSTM m => m (Channel tx)
newChannel = liftSTM newTQueue

newtype Subscription = Subscription Text
    deriving (Eq, Ord, Show)

type Subscribers ev = Map (Id (Channel ev)) (Channel ev)

data Evented ev prod = Evented
    { evProducer    :: prod
    , evSubscribers :: Subscribers ev
    }

evented :: Ord (Id (Channel ev)) => prod -> Evented ev prod
evented prod = Evented prod mempty

mapProducer
    :: (prod -> prod)
    -> Evented ev prod
    -> Evented ev prod
mapProducer = undefined

mapSubscribers
    :: (Subscribers ev -> Subscribers ev)
    -> Evented ev prod
    -> Evented ev prod
mapSubscribers f (Evented p subs) =
    Evented p (f subs)
