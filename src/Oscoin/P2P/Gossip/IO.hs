{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module Oscoin.P2P.Gossip.IO
    ( Callbacks (..)

    , Handle
    , HasHandle (..)
    , new

    , NetworkT
    , runNetworkT

    , Peer
    , knownPeer

    , listen
    , send
    , connect
    , disconnect
    ) where

import           Oscoin.Prelude

import           Oscoin.Logging (Logger, withExceptionLogged)
import qualified Oscoin.Logging as Log
import           Oscoin.P2P.Connection (Active, Connection(..), activeNew)
import qualified Oscoin.P2P.Connection as Conn
import           Oscoin.P2P.Gossip.Wire (WireMessage(..))
import           Oscoin.P2P.Handshake (Handshake)
import qualified Oscoin.P2P.Handshake as Handshake
import           Oscoin.P2P.Types (NodeId)

import           Codec.Serialise (Serialise(..))
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Concurrent (forkFinally)
import           Control.Monad (unless)
import           Control.Monad.Fail (fail)
import           Control.Monad.IO.Unlift
import           Data.Conduit (runConduit, transPipe, (.|))
import qualified Data.Conduit.Combinators as Conduit
import           Data.Has (Has(..))
import           Data.Hashable (Hashable(..), hashUsing)
import           Formatting (mapf, (%))
import           Lens.Micro (Lens', lens)
import           Lens.Micro.Mtl (view)
import           Network.Socket (AddrInfo(..), AddrInfoFlag(..), SocketType(..))
import qualified Network.Socket as Sock
import           Network.Socket.Serialise (decodeSockAddr, encodeSockAddr)

data Callbacks p = Callbacks
    { recvPayload    :: Peer -> p -> IO ()
    , connectionLost :: Peer -> IO ()
    }

data Handle e p p' = Handle
    { hLogger    :: Logger
    , hConns     :: Active (WireMessage p)
    , hHandshake :: Handshake e NodeId (WireMessage p) p'
    , hCallbacks :: Callbacks p
    }

class HasHandle a e p p' | a -> e, a -> p, a -> p' where
    handle :: Lens' a (Handle e p p')

instance HasHandle (Handle e p p') e p p' where
    handle = identity
    {-# INLINE handle #-}

instance Has Logger (Handle e p p') where
    hasLens = lens hLogger (\s a -> s { hLogger = a })
    {-# INLINE hasLens #-}

data NetworkError =
      ProtocolError
    | Gone
    deriving Show

instance Exception NetworkError

data Peer = Peer
    { peerNodeId :: NodeId
    , peerAddr   :: Sock.SockAddr
    } deriving (Eq, Show)

instance Serialise Peer where
    encode (Peer nid addr) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 0
        <> encode nid
        <> encodeSockAddr addr

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (3, 0) -> liftA2 Peer decode decodeSockAddr
            _      -> fail "CBOR Peer: invalid tag"

instance Hashable Peer where
    hashWithSalt salt (Peer nid addr) =
        (salt `hashWithSalt` nid) `hashAddr` addr
      where
        hashAddr s (Sock.SockAddrInet portNum hostAddr) =
            (s `hashWithSalt` (0 :: Word8))
               `hashPortNum`  portNum
               `hashWithSalt` hostAddr

        hashAddr s (Sock.SockAddrInet6 portNum flow hostAddr scope) =
            (s `hashWithSalt` (1 :: Word8))
               `hashPortNum`  portNum
               `hashWithSalt` flow
               `hashWithSalt` hostAddr
               `hashWithSalt` scope

        hashAddr s (Sock.SockAddrUnix path) =
            s `hashWithSalt` (2 :: Word8) `hashWithSalt` path

        -- hashAddr s (Sock.SockAddrCan x) = canNotSupported
        hashAddr _ _ = canNotSupported

        hashPortNum = hashUsing fromEnum

type NetworkT r = ReaderT r IO

runNetworkT :: r -> NetworkT r a -> IO a
runNetworkT = flip runReaderT

new :: Logger
    -> Handshake e NodeId (WireMessage p) p'
    -> Callbacks p
    -> IO (Handle e p p')
new hLogger hHandshake hCallbacks = do
    hConns <- atomically activeNew
    pure Handle{..}

knownPeer :: NodeId -> Sock.HostName -> Sock.PortNumber -> IO Peer
knownPeer nid host port = Peer nid <$> resolve
  where
    resolve = do
        let hints = Sock.defaultHints
                        { addrFlags      = [AI_ALL, AI_NUMERICSERV]
                        , addrSocketType = Stream
                        }
        addr:_ <- Sock.getAddrInfo (Just hints) (Just host) (Just (show port))
        pure $ addrAddress addr

listen
    :: ( Exception      e
       , Serialise          p'
       , HasHandle    r e p p'
       , Has Logger   r
       )
    => Sock.HostName
    -> Sock.PortNumber
    -> NetworkT r Void
listen host port = do
    addr <- liftIO $ resolve host port
    bracket (liftIO $ open addr) (liftIO . Sock.close) accept
  where
    resolve h p = do
        let hints = Sock.defaultHints
                        { addrFlags      = [AI_PASSIVE, AI_NUMERICSERV]
                        , addrSocketType = Stream
                        }
        addr:_ <- Sock.getAddrInfo (Just hints) (Just h) (Just (show p))
        pure addr

    open addr = do
        sock <- Sock.socket (Sock.addrFamily addr)
                            (Sock.addrSocketType addr)
                            (Sock.addrProtocol addr)
        Sock.setSocketOption sock Sock.ReuseAddr 1
        Sock.bind sock (Sock.addrAddress addr)
#if MIN_VERSION_network(2,7,0)
        Sock.setCloseOnExecIfNeeded $ Sock.fdSocket sock
#endif
        Sock.listen sock 10
        pure $! sock

    accept sock = do
        Handle{hLogger, hHandshake} <- view handle
        withRunInIO $ \run ->
            forever $ do
                (sock', addr) <- Sock.accept sock
                forkUltimately_ (Sock.close sock') $ do
                    conn <-
                        hHandshake
                            Handshake.Acceptor
                            (Conn.mkSocket' sock')
                            Nothing

                    case conn of
                        Left  e -> Log.logException hLogger e
                        Right c -> do
                            conn' <-
                                Conn.mkConnection
                                    (Handshake.hrPeerId c)
                                    sock'
                                    addr
                                    (Handshake.hrSend c)
                                    (Handshake.hrRecv c)
                            run $ recvAll conn'

send :: HasHandle r e p p'
     => Peer
     -> WireMessage p
     -> NetworkT r ()
send Peer{peerNodeId} msg = do
    Handle{hConns} <- view handle
    conn <- liftIO . atomically $ Conn.activeGet hConns peerNodeId
    case conn of
        Just c  -> liftIO $ connSendWire c msg
        Nothing -> throwM Gone

connect
    :: ( Exception    e
       , Serialise        p'
       , HasHandle  r e p p'
       , Has Logger r
       )
    => Peer
    -> NetworkT r ()
connect Peer{peerNodeId, peerAddr} = do
    Handle{hLogger, hConns, hHandshake} <- view handle

    withRunInIO $ \run -> do
        known <- atomically $ Conn.activeHas hConns peerNodeId
        unless known $ do
            sock <- Sock.socket (family peerAddr) Stream Sock.defaultProtocol
            conn <-
                flip onException (Sock.close sock) . withExceptionLogged hLogger $ do
                    Sock.connect sock peerAddr
                    hHandshake
                        Handshake.Connector
                        (Conn.mkSocket' sock)
                        (Just peerNodeId)
            case conn of
                Left  e -> throwM e
                Right c -> do
                    conn' <-
                        Conn.mkConnection
                            (Handshake.hrPeerId c)
                            sock
                            peerAddr
                            (Handshake.hrSend c)
                            (Handshake.hrRecv c)

                    forkUltimately_ (connClose conn') . run $
                        recvAll conn'
  where
    family Sock.SockAddrInet{}  = Sock.AF_INET
    family Sock.SockAddrInet6{} = Sock.AF_INET6
    family Sock.SockAddrUnix{}  = Sock.AF_UNIX
    --family Sock.SockAddrCan{}   = Sock.AF_CAN
    family _                    = canNotSupported

disconnect :: HasHandle r e p p' => Peer -> NetworkT r ()
disconnect Peer{peerNodeId} = do
    Handle{hConns} <- view handle
    liftIO $ do
        conn <- atomically $ Conn.activeDel hConns peerNodeId
        for_ conn connClose

--------------------------------------------------------------------------------

recvAll
    :: ( HasHandle  r e p p'
       , Has Logger r
       )
    => Connection (WireMessage p)
    -> NetworkT r ()
recvAll conn = do
    Handle { hConns
           , hCallbacks = Callbacks {connectionLost, recvPayload}
           } <- view handle

    ok <- liftIO . atomically $ Conn.activeAdd hConns conn
    if ok then
        onException (runConduit $ recv conn recvPayload) . liftIO $ do
            atomically $ Conn.activeDel_ hConns conn
            connectionLost $ toPeer conn
    else
        goaway conn "Duplicate Node Id"
  where
    goaway c msg = do
        Log.errM Log.stext msg
        liftIO $ connSendWire c (WireGoaway (pure msg))

    recv c recvPayload =
        transPipe liftIO (connRecvWire c) .| Conduit.mapM_ (\case
            WirePayload p  -> liftIO $ recvPayload (toPeer c) p
            WireGoaway msg -> Log.errM fgoaway "GOAWAY received: " msg
                           *> throwM ProtocolError)

    toPeer c = Peer { peerNodeId = connNodeId c, peerAddr = connAddr c }

    fgoaway = Log.stext % mapf (fromMaybe mempty) Log.stext

forkUltimately_ :: IO () -> IO a -> IO ()
forkUltimately_ fin work = void $ forkFinally work (const $ fin)

canNotSupported :: HasCallStack => a
canNotSupported = panic "CAN addresses not supported"
