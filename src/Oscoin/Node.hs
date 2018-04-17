module Oscoin.Node where

import           Oscoin.Node.Service
import qualified Oscoin.Node.State.Mempool as Mempool
import qualified Oscoin.Node.State.Tree as STree

import           Oscoin.Prelude
import           Oscoin.Environment
import           Oscoin.Account (Account, AccId)
import qualified Oscoin.HTTP as HTTP
import qualified Oscoin.P2P as P2P
import qualified Oscoin.Consensus as Consensus

import           Control.Concurrent.Async
import qualified Network.Socket as NS

data Config = Config
    { cfgServiceName :: NS.ServiceName
    , cfgPeers       :: [(NS.HostName, NS.ServiceName)]
    , cfgEnv         :: Environment
    , cfgAccounts    :: [(AccId, Account)]
    }

data State = State ()

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
