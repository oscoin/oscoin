-- | Naive UDP multicast discovery
module Oscoin.P2P.Discovery.Multicast
    ( Config (..)
    , mkConfig

    , mkDisco
    ) where

import           Oscoin.Prelude

import           Oscoin.Logging (Logger)
import qualified Oscoin.Logging as Log
import           Oscoin.P2P.Discovery.Internal (Disco(..))
import           Oscoin.P2P.Types (Endpoints, NodeId)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Monad (forever, unless)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Network.Multicast as NMC
import           Network.Socket (HostName, PortNumber)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

data Config = Config
    { cfgMcastDiscoGroup    :: HostName
    , cfgMcastDiscoPort     :: PortNumber
    , cfgPacketSize         :: Int
    , cfgAdvertiseId        :: NodeId
    , cfgAdvertiseEndpoints :: Endpoints
    , cfgAdvertiseEverySecs :: Int
    }

mkConfig :: NodeId -> Endpoints -> Config
mkConfig id srvs = Config
    { cfgMcastDiscoGroup    = "226.111.111.104"
    , cfgMcastDiscoPort     = 8008
    , cfgPacketSize         = 65536
    , cfgAdvertiseId        = id
    , cfgAdvertiseEndpoints = srvs
    , cfgAdvertiseEverySecs = 5
    }

mkDisco :: Logger -> Config -> IO (Disco IO)
mkDisco lgr cfg = do
    kp <- newTVarIO mempty
    let lgr' = Log.setNamespace (Just "mcast-disco") lgr
    th <- async $ race_ (advertise cfg lgr') (discover cfg lgr' kp)

    pure Disco
        { knownPeers = readTVarIO kp
        , closeDisco = uninterruptibleCancel th
        }

-- Internal --------------------------------------------------------------------

-- TODO(kim): should preferably advertise some (random) node id
-- | Advertise the local listening address on the network.
--
-- This exists mainly to identify ourselves, such that 'knownPeers' does not
-- include the local node.
data Beacon = Beacon
    { myId        :: NodeId
    , myEndpoints :: Endpoints
    } deriving Generic

instance Binary Beacon

advertise :: Config -> Logger -> IO ()
advertise Config{..} lgr = NS.withSocketsDo $ do
    (sock, addr) <- NMC.multicastSender cfgMcastDiscoGroup cfgMcastDiscoPort
    forever $ do
        Log.debug lgr "Advertising"
        NSB.sendManyTo sock msg addr
        threadDelay (cfgAdvertiseEverySecs * 1000000)
  where
    msg = LBS.toChunks .  Binary.encode $ Beacon
        { myId        = cfgAdvertiseId
        , myEndpoints = cfgAdvertiseEndpoints
        }

discover :: Config -> Logger -> TVar (Map NodeId Endpoints) -> IO ()
discover Config{..} lgr tvar = NS.withSocketsDo $ do
    sock <- NMC.multicastReceiver cfgMcastDiscoGroup cfgMcastDiscoPort
    forever $ do
        (msg, _) <- NSB.recvFrom sock cfgPacketSize
        Log.debug lgr "Received beacon"
        let Beacon id addrs = Binary.decode $ LBS.fromStrict msg
        unless (id == cfgAdvertiseId) $
            atomically . modifyTVar' tvar $
                Map.insert id addrs
