{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Oscoin.Node.Service
    ( ServiceT(..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus as Consensus
import           Oscoin.Environment
import qualified Oscoin.HTTP as HTTP
import           Oscoin.Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Tree as STree
import qualified Oscoin.P2P as P2P

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar (TVar)

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
    type H sv :: *
    type Cfg sv :: *

    start :: Environment -> Cfg sv -> sv (H sv)
    stop :: H sv -> sv ()

type NodeT m a = ServiceT State Config m a

run :: Config -> NodeT IO ()
run Config{..} = do
    mp <- Mempool.new
    st <- lift STree.connect
    threads <- lift . traverse async $
        [ HTTP.run (HTTP.api cfgEnv) cfgAccounts (readStr cfgServiceName) mp st
        , P2P.run cfgEnv mp st
        , runReaderT (Consensus.run cfgEnv st) mp
        ]
    (_, _err) <- lift $ waitAny threads
    pass
