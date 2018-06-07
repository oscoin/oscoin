module Oscoin.P2P.Simple (runProto) where

import           Oscoin.P2P.Discovery
import           Oscoin.Prelude
import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Simple
import           Oscoin.Crypto.Blockchain.Block (genesisBlock)

import           Control.Concurrent.Async (async, waitAny)
import           Control.Concurrent (newMVar, takeMVar, putMVar, MVar, threadDelay)
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket as NS
import           Data.Binary (Binary)
import qualified Data.Binary as Binary (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock.POSIX (getPOSIXTime)

packetSize :: Int
packetSize = 65536

second :: Int
second = 1000000

sendMessages
    :: Binary msg
    => [(NS.SockAddr, msg)] -> NS.Socket -> IO ()
sendMessages outbound s =
    for_ outbound (\(to, out) -> do
        let serialized = LBS.toStrict $ Binary.encode out
        NSB.sendAllTo s serialized to)

runPeriodic
    :: forall p . p ~ SimpleNode Text
    => NS.Socket -> MVar p -> IO ()
runPeriodic s protoVar = do
    proto <- takeMVar protoVar
    now <- getPOSIXTime
    let (proto', outbound) = step proto now Nothing
    putMVar protoVar proto'

    sendMessages outbound s

    let sleep = epoch proto'
    threadDelay (toSeconds sleep * second)

    runPeriodic s protoVar

runListener
    :: ( Show (Msg p)
       , Binary (Msg p)
       , Addr p ~ NS.SockAddr
       , Protocol p )
    => NS.Socket -> MVar p -> IO ()
runListener s protoVar = do
    (bytes, from) <- NSB.recvFrom s packetSize
    let deserialized = Binary.decode $ LBS.fromStrict bytes

    proto <- takeMVar protoVar
    now <- getPOSIXTime
    let (proto', outbound) = step proto now (Just (from, deserialized))
    putMVar protoVar proto'

    sendMessages outbound s

    runListener s protoVar

runProto :: NS.SockAddr -> NS.PortNumber -> IO ()
runProto addr port = do
    let proto = SimpleNode
                { snAddr    = addr
                , snPeers   = mempty
                , snBuffer  = mempty
                , snLastBlk = 0
                , snLastAsk = 0
                , snStore   = genesisBlockStore (genesisBlock 0 [])
                } :: SimpleNode Text

    protoVar <- newMVar proto

    s <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
    NS.bind s addr

    cron       <- async $ runPeriodic s protoVar
    receiver   <- async $ runListener s protoVar
    advertiser <- async $ localAdvertiser port
    discovery  <- async $ localPeerDiscovery protoVar

    waitAny [ cron, receiver, advertiser, discovery ]

    pure ()
