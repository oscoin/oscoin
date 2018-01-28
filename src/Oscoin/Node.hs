module Oscoin.Node where

import           Oscoin.Node.Service

import           Oscoin.Prelude
import           Oscoin.Environment
import qualified Oscoin.HTTP as HTTP

import qualified Network.Socket as NS

data Config = Config
    { cfgServiceName :: NS.ServiceName
    , cfgPeers       :: [(NS.HostName, NS.ServiceName)]
    , cfgEnv         :: Environment
    }

data State = State ()

type NodeT m a = ServiceT State Config m a

run :: Config -> NodeT IO ()
run Config{..} =
    lift $ HTTP.run cfgEnv (read cfgServiceName)
