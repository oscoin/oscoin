module Oscoin.Consensus.Test.View
    ( DummyView(..)
    , DummyTx
    , runDummyView
    ) where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class

import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import           Data.List (sort)

-- DummyView ------------------------------------------------------------------

type DummyTx = Word8

newtype DummyView (f :: * -> *) tx a = DummyView (State (f tx) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState (f tx)
             )

runDummyView :: DummyView f tx a -> f tx -> (a, f tx)
runDummyView (DummyView inner) xs =
    runState inner xs

instance Context (DummyView f tx) where
    type T   (DummyView f tx) = f tx
    type Key (DummyView f tx) = ()

    get = notImplemented
    set = notImplemented
    del = notImplemented

instance Ord tx => View (DummyView [] tx) where
    type Transaction (DummyView [] tx) = tx
    type TransactionContext (DummyView [] tx) = ()

    apply _ txs =
        for_ txs $ \tx ->
            State.modify (\txs -> sort $ tx : txs)

-------------------------------------------------------------------------------

