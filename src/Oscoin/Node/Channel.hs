module Oscoin.Node.Channel where

import Oscoin.Prelude
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, writeTQueue, readTQueue, flushTQueue)
import qualified Data.Map as Map

newtype Event tx = Event { fromEvent :: tx }
    deriving (Functor, Eq, Ord, Show)

type Channel tx = TQueue (Event tx)

type instance Id (Channel tx) = Subscription tx

newChannel :: MonadSTM m => m (Channel tx)
newChannel = liftSTM newTQueue

readChannel :: MonadSTM m => Channel tx -> m (Event tx)
readChannel =
    liftSTM . readTQueue

flushChannel :: MonadSTM m => Channel tx -> m [Event tx]
flushChannel =
    liftSTM . flushTQueue

newtype Subscription ev = Subscription Text
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
mapProducer f (Evented p subs) =
    Evented (f p) subs

mapSubscribers
    :: (Subscribers ev -> Subscribers ev)
    -> Evented ev prod
    -> Evented ev prod
mapSubscribers f (Evented p subs) =
    Evented p (f subs)

notifySubscribers
    :: (Monad m, MonadSTM m)
    => Evented ev prod
    -> ev
    -> m ()
notifySubscribers (Evented{evSubscribers}) ev =
    for_ (Map.toList evSubscribers) $ \(_, chan) ->
        liftSTM $ writeTQueue chan (Event ev)
