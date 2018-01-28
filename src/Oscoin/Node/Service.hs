module Oscoin.Node.Service where

import Oscoin.Prelude
import Control.Concurrent.STM.TVar (TVar)

newtype ServiceT g l m a = ServiceT (ReaderT (TVar g, l) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (TVar g, l)
             , MonadTrans
             )

