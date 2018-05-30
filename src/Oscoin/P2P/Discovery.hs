module Oscoin.P2P.Discovery ( localAdvertiser
                            , localPeerDiscovery
                            ) where

import Oscoin.Prelude
import Oscoin.Consensus.Class
import Oscoin.Consensus.Simple

import           Control.Concurrent
import qualified Network.Multicast as NMC
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket as NS
import           Data.Binary (Binary)
import qualified Data.Binary as Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString (ByteString)
import qualified Data.Set as Set

packetSize :: Int
packetSize = 65536

second :: Int
second = 1000000

replacePort :: NS.SockAddr -> NS.PortNumber -> NS.SockAddr
replacePort (NS.SockAddrInet _ host) port =
    NS.SockAddrInet port host
replacePort (NS.SockAddrInet6 _ flow host scope) port =
    NS.SockAddrInet6 port flow host scope
replacePort other _ =
    other

localAdvertiser :: NS.PortNumber -> IO ()
localAdvertiser port = NS.withSocketsDo $ do
    (sock, addr) <- NMC.multicastSender "226.111.111.104" 8008
    advertiserLoop sock addr msg
  where
    pn  = fromIntegral port :: Int
    msg = LBS.toStrict $ Binary.encode pn

advertiserLoop :: NS.Socket -> NS.SockAddr -> ByteString -> IO ()
advertiserLoop sock addr msg = do
    NSB.sendTo sock msg addr
    threadDelay second
    advertiserLoop sock addr msg

localPeerDiscovery :: ( Binary (Msg (SimpleNode tx))
                      , Addr (SimpleNode tx) ~ NS.SockAddr
                      , Protocol (SimpleNode tx)
                      ) => MVar (SimpleNode tx) -> IO ()
localPeerDiscovery mvar = NS.withSocketsDo $ do
    sock <- NMC.multicastReceiver "226.111.111.104" 8008
    discoveryLoop mvar sock

discoveryLoop :: ( Binary (Msg (SimpleNode tx))
                 , Addr (SimpleNode tx) ~ NS.SockAddr
                 , Protocol (SimpleNode tx)
                 ) => MVar (SimpleNode tx) -> NS.Socket -> IO ()
discoveryLoop mvar sock = do
    (msg, addr) <- NSB.recvFrom sock packetSize
    let pn = Binary.decode $ LBS.fromStrict msg :: Int
    let port = fromIntegral pn :: NS.PortNumber
    let addr' = replacePort addr port
    proto <- takeMVar mvar
    let peers = snPeers proto
    let peers' = if addr' == (snAddr proto)
        then peers
        else Set.toList $ Set.insert addr' $ Set.fromList peers
    let proto' = proto { snPeers = peers' }
    putMVar mvar proto'
    discoveryLoop mvar sock
