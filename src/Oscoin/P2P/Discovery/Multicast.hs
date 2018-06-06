-- | Naive UDP multicast discovery
module Oscoin.P2P.Discovery.Multicast
    ( Config (..)
    , mkConfig

    , mkDisco
    ) where

import           Oscoin.P2P.Discovery.Internal (Disco(..))
import           Oscoin.Prelude

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.Monad (forever, unless)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Network.Multicast as NMC
import           Network.Socket (HostName, PortNumber, SockAddr(..))
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

data Config = Config
    { cfgMcastDiscoGroup    :: HostName
    , cfgMcastDiscoPort     :: PortNumber
    , cfgPacketSize         :: Int
    , cfgAdvertiseHost      :: HostName
    , cfgAdvertisePort      :: PortNumber
    , cfgAdvertiseEverySecs :: Int
    }

mkConfig :: PortNumber -> Config
mkConfig p = Config
    { cfgMcastDiscoGroup    = "226.111.111.104"
    , cfgMcastDiscoPort     = 8008
    , cfgPacketSize         = 65536
    , cfgAdvertiseHost      = "127.0.0.1"
    , cfgAdvertisePort      = p
    , cfgAdvertiseEverySecs = 10
    }

mkDisco :: Config -> IO (Disco IO SockAddr)
mkDisco s = do
    kp <- newTVarIO mempty
    th <- async $ race_ (advertise s) (discover s kp)

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
    { myHost :: HostName
    , myPort :: Int
    } deriving Generic

instance Binary Beacon

advertise :: Config -> IO ()
advertise Config{..} = NS.withSocketsDo $ do
    (sock, addr) <- NMC.multicastSender cfgMcastDiscoGroup cfgMcastDiscoPort
    forever $ do
        NSB.sendManyTo sock msg addr
        threadDelay (cfgAdvertiseEverySecs * 1000000)
  where
    msg = LBS.toChunks .  Binary.encode $ Beacon
        { myHost = cfgAdvertiseHost
        , myPort = fromIntegral cfgAdvertisePort
        }

discover :: Config -> TVar (Set SockAddr) -> IO ()
discover Config{..} tvar = NS.withSocketsDo $ do
    sock <- NMC.multicastReceiver cfgMcastDiscoGroup cfgMcastDiscoPort
    forever $ do
        (msg, addr) <- NSB.recvFrom sock cfgPacketSize
        let Beacon h (fromIntegral -> p) = Binary.decode $ LBS.fromStrict msg
        unless (h == cfgAdvertiseHost && p == cfgAdvertisePort) $
            atomically . modifyTVar' tvar $
                Set.insert (replacePort addr p)
  where
    replacePort :: SockAddr -> PortNumber -> SockAddr
    replacePort (SockAddrInet _ host) port =
        SockAddrInet port host
    replacePort (SockAddrInet6 _ flow host scope) port =
        SockAddrInet6 port flow host scope
    replacePort other _ =
        other
