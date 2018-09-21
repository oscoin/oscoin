{-# LANGUAGE DefaultSignatures #-}

module Oscoin.P2P
    ( Config (..)
    , Handle
    , Msg (..)

    , MonadNetwork (..)

    , NetworkT
    , NetworkIO

    , runNetworkT

    , defaultConfig
    , withP2P
    , open
    , close
    , send
    , receive

    -- * New Gossip
    , runGossip
    , broadcast

    -- * Re-exports
    , module Oscoin.P2P.Types
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (MonadClock(..))
import           Oscoin.Crypto.Blockchain (showBlockDigest)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Environment
import           Oscoin.Logging (Logger, shown, withExceptionLogged, (%))
import qualified Oscoin.Logging as Log
import           Oscoin.P2P.Discovery (Disco(..))
import qualified Oscoin.P2P.Gossip as Gossip
import           Oscoin.P2P.Gossip.Broadcast (broadcast)
import qualified Oscoin.P2P.Gossip.Broadcast as Bcast
import qualified Oscoin.P2P.Gossip.Handshake as Handshake
import qualified Oscoin.P2P.Gossip.Membership as Membership
import           Oscoin.P2P.Types

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Control.Concurrent.Async as Async
import           Control.Exception.Safe
import           Data.ByteString.Lazy (fromStrict, toChunks)
import           Data.IP (IP)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetBS

data Config = Config
    { cfgEnvironment :: Environment
    , cfgBindIP      :: IP
    , cfgBindPort    :: Word16
    }

data Handle = Handle
    { hEnvironment :: Environment
    , hLogger      :: Logger
    , hDiscovery   :: Disco IO
    , hSocket      :: Net.Socket
    }

data Msg tx =
      BlockMsg    (Block tx ())
    | TxMsg       [tx]
    | ReqBlockMsg BlockHash
    deriving (Eq, Generic)

instance Show tx => Show (Msg tx) where
    show (BlockMsg  blk) = "BlockMsg " ++ showBlockDigest blk
    show (TxMsg     txs) = "TxMsg " ++ show txs
    show (ReqBlockMsg h) = "ReqBlockMsg " ++ show h

instance Serialise tx => Serialise (Msg tx)

class Monad m => MonadNetwork tx m | m -> tx where
    sendM :: Foldable t => t (Msg tx) -> m ()
    recvM :: m (Msg tx)

    default sendM
        :: (MonadNetwork tx m', MonadTrans t, m ~ t m', Foldable f)
        => f (Msg tx) -> m ()
    sendM = lift . sendM
    {-# INLINE sendM #-}

    default recvM
        :: (MonadNetwork tx m', MonadTrans t, m ~ t m')
        => m (Msg tx)
    recvM = lift recvM
    {-# INLINE recvM #-}

newtype NetworkT tx m a = NetworkT (ReaderT Handle m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Handle
             , MonadTrans
             , MonadIO
             )

type NetworkIO tx = NetworkT tx IO

instance (Serialise tx, Show tx, MonadIO m) => MonadNetwork tx (NetworkT tx m) where
    sendM msgs = ask >>= io . (`send` msgs)
    recvM      = ask >>= io . receive

instance MonadClock m => MonadClock (NetworkT tx m)

runNetworkT :: Handle -> NetworkT tx m a -> m a
runNetworkT h (NetworkT ma) = runReaderT ma h

--------------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
    { cfgEnvironment = Testing
    , cfgBindIP      = read "127.0.0.1"
    , cfgBindPort    = 4269
    }

withP2P :: Config -> Logger -> Disco IO -> (Handle -> IO a) -> IO a
withP2P cfg lgr disco = bracket (open cfg lgr disco) close

type GossipHandle = Gossip.Handle Handshake.HandshakeError Gossip.Peer

-- | Start listening to gossip and pass the gossip handle to the
-- runner. If the runner returns we stop listening and return the
-- runner result.
runGossip
    :: Logger
    -> Crypto.KeyPair
    -> NodeId
    -> Net.HostName
    -- ^ Host to bind to
    -> Net.PortNumber
    -- ^ Port to bind to
    -> [Gossip.Peer]
    -- ^ Initial peers to connect to
    -> Bcast.Callbacks
    -> (GossipHandle -> IO a)
    -> IO a
runGossip logger keypair nodeId host port initialPeers broadcastCallbacks run = do
    self :: Gossip.Peer <- Gossip.knownPeer nodeId host port
    Gossip.withGossip
        logger
        keypair
        self
        scheduleInterval
        broadcastCallbacks
        Handshake.simple
        Membership.defaultConfig
        listenAndRun
  where
    listen = runReaderT $ Gossip.listen host port initialPeers
    listenAndRun gossipHandle =
        Async.withAsync (listen gossipHandle) (\_ -> run gossipHandle)
    scheduleInterval = 10


open :: Config -> Logger -> Disco IO -> IO Handle
open Config{..} hLogger hDiscovery = do
    let hEnvironment = cfgEnvironment
    hSocket <- mkSocket
    pure Handle{..}
  where
    mkSocket = do
        let hints = Net.defaultHints { Net.addrSocketType = Net.Datagram }
        addr:_ <- Net.getAddrInfo (Just hints)
                                  (Just (show cfgBindIP))
                                  (Just (show cfgBindPort))
        sock   <- Net.socket (Net.addrFamily addr)
                             (Net.addrSocketType addr)
                             (Net.addrProtocol addr)
        Net.bind sock (Net.addrAddress addr)
        pure sock

close :: Handle -> IO ()
close Handle{hLogger, hSocket} =
    withExceptionLogged hLogger (Net.close hSocket)

send :: (Serialise tx, Foldable t) => Handle -> t (Msg tx) -> IO ()
send Handle{hDiscovery, hSocket} msgs = do
    let payloads = map (toChunks . Serialise.serialise) (toList msgs)
    peers <- knownPeers hDiscovery
    for_ peers $ \peer ->
        for_ payloads $ \payload ->
            NetBS.sendManyTo hSocket payload (addr peer)
  where
    addr = toSockAddr . p2pEndpoint

receive :: (Show tx, Serialise tx) => Handle -> IO (Msg tx)
receive Handle{hLogger, hSocket} = loop
  where
    loop = do
        (pkt,_) <- NetBS.recvFrom hSocket 1024
        case Serialise.deserialiseOrFail (fromStrict pkt) of
            Left  _         -> do
                Log.err hLogger "Dropping invalid packet"
                loop
            Right msg -> do
                Log.info hLogger ("Received: " % shown) msg
                pure msg
