module Oscoin.Node where

import           Oscoin.Node.Service

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.HTTP as HTTP
import qualified Oscoin.P2P as P2P
import qualified Oscoin.Consensus as Consensus

import           Control.Concurrent.Async
import qualified Network.Socket as NS

data Config = Config
    { cfgServiceName :: NS.ServiceName
    , cfgPeers       :: [(NS.HostName, NS.ServiceName)]
    , cfgEnv         :: Environment
    }

data State = State ()

type NodeT m a = ServiceT State Config m a

run :: Config -> NodeT IO ()
run Config{..} = do
    threads <- lift . traverse async $
        [ HTTP.run cfgEnv (read cfgServiceName)
        , P2P.run cfgEnv
        , Consensus.run cfgEnv
        ]
    (_, _err) <- lift $ waitAny threads
    pass
