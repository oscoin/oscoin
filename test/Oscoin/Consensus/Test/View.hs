module Oscoin.Consensus.Test.View
    ( DummyView(..)
    , DummyTx
    , runDummyView
    ) where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class

import           Control.Monad.State (State)
import qualified Control.Monad.State as State

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

instance MonadQuery (DummyView f tx) where
    type Key (DummyView f tx) = ()
    type Val (DummyView f tx) = ()

    queryM = notImplemented

instance MonadModify (DummyView f tx) where
    setM = notImplemented
    delM = notImplemented

-------------------------------------------------------------------------------

