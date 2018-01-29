{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Oscoin.Node.Service
    ( ServiceT(..)
    ) where

import Oscoin.Prelude
import Oscoin.Environment
import Control.Concurrent.STM.TVar (TVar)

-- | A Service is a monad which can read and write to a global environment @g@
-- and read from a local environment @l@.
newtype ServiceT g l m a = ServiceT (ReaderT (TVar g, l) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (TVar g, l)
             , MonadTrans
             )

-- For now, this is purely here for documentation purposes, as this is how we
-- want to think about services.
class Service sv where
    type Handle sv :: *
    type Cfg sv :: *

    start :: Environment -> Cfg sv -> sv (Handle sv)
    stop :: Handle sv -> sv ()
