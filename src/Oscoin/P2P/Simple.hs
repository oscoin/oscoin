module Oscoin.P2P.Simple (runProto) where

import           Oscoin.Consensus.BlockStore (genesisBlockStore)
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.Simple
import           Oscoin.Crypto.Blockchain.Block (genesisBlock)
import           Oscoin.P2P.Discovery
import qualified Oscoin.P2P.Discovery.Multicast as MCast
import           Oscoin.Prelude

import           Control.Concurrent (MVar, newMVar, putMVar, takeMVar, threadDelay, withMVar)
import           Control.Concurrent.Async (race_)
import           Control.Monad (forever)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

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
runPeriodic s protoVar = forever $ do
    proto <- takeMVar protoVar
    now <- getPOSIXTime
    let (proto', outbound) = step proto now Nothing
    putMVar protoVar proto'

    sendMessages outbound s

    let sleep = epoch proto'
    threadDelay (toSeconds sleep * second)

runListener
    :: ( Binary (Msg p)
       , Addr p ~ NS.SockAddr
       , Protocol p )
    => NS.Socket
    -> MVar p
    -> IO ()
runListener s protoVar = forever $ do
    (bytes, from) <- NSB.recvFrom s packetSize
    let deserialized = Binary.decode $ LBS.fromStrict bytes

    proto <- takeMVar protoVar
    now   <- getPOSIXTime
    let (proto', outbound) = step proto now (Just (from, deserialized))
    putMVar protoVar proto'

    sendMessages outbound s

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

    let cron      = runPeriodic s protoVar
    let receiver  = runListener s protoVar
    let discovery = runDisco protoVar

    race_ discovery (race_ cron receiver)
  where
    runDisco protoVar =
        withDisco (MCast.mkDisco (MCast.mkConfig port)) $ \disco ->
            forever $ do
                peers <- knownPeers disco
                withMVar protoVar $ \p ->
                    pure $ p { snPeers = toList peers }
                threadDelay (3 * second)

