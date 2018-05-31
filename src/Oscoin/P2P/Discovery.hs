module Oscoin.P2P.Discovery
    ( localAdvertiser
    , localPeerDiscovery
    ) where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Simple

import           Control.Concurrent
import qualified Network.Multicast as NMC
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket as NS
import           Data.Binary (Binary)
import qualified Data.Binary as Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString (ByteString)

mcastDiscoGroup :: NS.HostName
mcastDiscoGroup = "226.111.111.104"

mcastDiscoPort :: NS.PortNumber
mcastDiscoPort = 8008

packetSize :: Int
packetSize = 65536

second :: Int
second = 1000000

localAdvertiser :: NS.PortNumber -> IO ()
localAdvertiser port = NS.withSocketsDo $ do
    (sock, addr) <- NMC.multicastSender mcastDiscoGroup mcastDiscoPort
    advertiserLoop sock addr msg
  where
    pn  = fromIntegral port :: Int
    msg = LBS.toStrict $ Binary.encode pn

advertiserLoop :: NS.Socket -> NS.SockAddr -> ByteString -> IO ()
advertiserLoop sock addr msg = do
    NSB.sendTo sock msg addr
    threadDelay second
    advertiserLoop sock addr msg

localPeerDiscovery
    :: ( Binary (Msg (SimpleNode tx))
       , Addr (SimpleNode tx) ~ NS.SockAddr
       , Protocol (SimpleNode tx) )
    => MVar (SimpleNode tx)
    -> IO ()
localPeerDiscovery mvar = NS.withSocketsDo $ do
    sock <- NMC.multicastReceiver mcastDiscoGroup mcastDiscoPort
    discoveryLoop mvar sock

discoveryLoop
    :: ( Binary (Msg (SimpleNode tx))
       , Addr (SimpleNode tx) ~ NS.SockAddr
       , Protocol (SimpleNode tx) )
    => MVar (SimpleNode tx)
    -> NS.Socket
    -> IO ()
discoveryLoop mvar sock = do
    (msg, addr) <- NSB.recvFrom sock packetSize
    let port  = fromIntegral (Binary.decode $ LBS.fromStrict msg :: Int)
        addr' = replacePort addr port
     in modifyMVar_ mvar (pure . addPeer addr')
    discoveryLoop mvar sock
  where
    replacePort :: NS.SockAddr -> NS.PortNumber -> NS.SockAddr
    replacePort (NS.SockAddrInet _ host) port =
        NS.SockAddrInet port host
    replacePort (NS.SockAddrInet6 _ flow host scope) port =
        NS.SockAddrInet6 port flow host scope
    replacePort other _ =
        other

