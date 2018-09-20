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

import           Oscoin.Crypto.PubKey (PrivateKey, PublicKey)
import           Oscoin.Logging (Logger, withExceptionLogged)
import qualified Oscoin.Logging as Log
import           Oscoin.P2P.Gossip.Connection
                 (Active, Connection(..), activeNew)
import qualified Oscoin.P2P.Gossip.Connection as Conn
import           Oscoin.P2P.Gossip.Handshake (Handshake(..))
import           Oscoin.P2P.Gossip.Wire (WireMessage(..))
import           Oscoin.P2P.Types (NodeId)

import           Codec.Serialise (Serialise(..))
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Concurrent (forkFinally)
import           Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as E
import           Control.Monad (unless)
import           Control.Monad.IO.Unlift
import           Data.Conduit (runConduit, transPipe, (.|))
import qualified Data.Conduit.Combinators as Conduit
import           Data.Hashable
import           Data.Void
import           Formatting (mapf, (%))
import           Lens.Micro (Lens', lens)
import           Lens.Micro.Mtl (view)
import           Network.Socket (AddrInfo(..), AddrInfoFlag(..), SocketType(..))
import qualified Network.Socket as Sock

data Callbacks p = Callbacks
    { recvPayload    :: Peer -> p -> IO ()
    , connectionLost :: Peer -> IO ()
    }

data Handle e p = Handle
    { hLogger    :: Logger
    , hKeys      :: (PublicKey, PrivateKey)
    , hConns     :: Active p
    , hHandshake :: Handshake e p
    , hCallbacks :: Callbacks p
    }

class HasHandle a e p | a -> e, a -> p where
    handle :: Lens' a (Handle e p)

instance HasHandle (Handle e p) e p where
    handle = identity
    {-# INLINE handle #-}

instance Has Logger (Handle e p) where
    hasLens = lens hLogger (\s a -> s { hLogger = a })
    {-# INLINE hasLens #-}

data NetworkError =
      ProtocolError
    | Gone
    | IdMismatch Peer NodeId
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
        <> encodeAddr addr

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (3, 0) -> liftA2 Peer decode decodeAddr
            _      -> fail "CBOR Peer: invalid tag"

encodeAddr :: Sock.SockAddr -> CBOR.Encoding
encodeAddr = \case
    Sock.SockAddrInet portNum hostAddr ->
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 0
        <> encodePort portNum
        <> encodeHost hostAddr

    Sock.SockAddrInet6 portNum flow hostAddr scope ->
           CBOR.encodeListLen 5
        <> CBOR.encodeWord 1
        <> encodePort portNum
        <> encode flow
        <> encodeHost6 hostAddr
        <> encode scope

    Sock.SockAddrUnix path ->
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 2
        <> encode path

    -- Sock.SockAddrCan{} ->
    _ -> canNotSupported
  where
    encodePort  = encode . fromEnum
    encodeHost  = encode . Sock.hostAddressToTuple
    encodeHost6 = encode . Sock.hostAddress6ToTuple

decodeAddr :: CBOR.Decoder s Sock.SockAddr
decodeAddr = do
    pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
    case pre of
        (3, 0) -> liftA2 Sock.SockAddrInet decodePort decodeHost
        (5, 1) -> Sock.SockAddrInet6
              <$> decodePort
              <*> decode
              <*> decodeHost6
              <*> decode
        (2, 2) -> Sock.SockAddrUnix <$> decode
        _ -> fail canNotSupported
  where
    decodePort  = toEnum <$> decode
    decodeHost  = Sock.tupleToHostAddress <$> decode
    decodeHost6 = Sock.tupleToHostAddress6 <$> decode

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
    -> (PublicKey, PrivateKey)
    -> Handshake e p
    -> Callbacks p
    -> IO (Handle e p)
new hLogger hKeys hHandshake hCallbacks = do
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
       , HasHandle    r e p
       , Has Logger   r
       )
    => Sock.HostName
    -> Sock.PortNumber
    -> NetworkT r Void
listen host port = do
    addr <- io $ resolve host port
    E.bracket (io $ open addr) (io . Sock.close) accept
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
        Handle{hLogger, hHandshake, hKeys} <- view handle
        withRunInIO $ \run ->
            forever $ do
                (sock', addr) <- Sock.accept sock
                forkUltimately_ (Sock.close sock') $ do
                    conn <- handshakeAcceptor hHandshake hKeys sock' addr
                    case conn of
                        Left  e -> Log.logException hLogger e
                        Right c -> run $ recvAll c

-- TODO(kim): do we need to mutex send?
send :: HasHandle  r e p
     => Peer
     -> WireMessage p
     -> NetworkT r ()
send Peer{peerNodeId} msg = do
    Handle{hConns} <- view handle
    conn <- io . atomically $ Conn.activeGet hConns peerNodeId
    case conn of
        Just c  -> io $ connSendWire c msg
        Nothing -> E.throwM Gone

connect :: (Exception e, HasHandle r e p, Has Logger r) => Peer -> NetworkT r ()
connect peer@Peer{peerNodeId, peerAddr} = do
    Handle{hLogger, hKeys, hConns, hHandshake} <- view handle

    withRunInIO $ \run -> do
        known <- atomically $ Conn.activeHas hConns peerNodeId
        unless known $ do
            sock <- Sock.socket (family peerAddr) Stream Sock.defaultProtocol
            conn <-
                flip E.onException (Sock.close sock) . withExceptionLogged hLogger $ do
                    Sock.connect sock peerAddr
                    handshakeInitiator hHandshake hKeys sock peerAddr
            case conn of
                Left  e -> E.throwM e
                Right c
                    | nid <- connNodeId c
                    , nid /= peerNodeId ->
                        flip E.finally (Sock.close sock) $ do
                            connSendWire c (WireGoaway (pure "Id Mismatch"))
                                `E.catchAny` const (pure ())
                            E.throwM $ IdMismatch peer nid
                    | otherwise ->
                        forkUltimately_ (Sock.close sock) $
                            run $ recvAll c
  where
    family Sock.SockAddrInet{}  = Sock.AF_INET
    family Sock.SockAddrInet6{} = Sock.AF_INET6
    family Sock.SockAddrUnix{}  = Sock.AF_UNIX
    --family Sock.SockAddrCan{}   = Sock.AF_CAN
    family _                    = canNotSupported

disconnect :: HasHandle r e p => Peer -> NetworkT r ()
disconnect Peer{peerNodeId} = do
    Handle{hConns} <- view handle
    io $ do
        conn <- atomically $ Conn.activeDel hConns peerNodeId
        for_ conn connClose

--------------------------------------------------------------------------------

recvAll :: (HasHandle r e p, Has Logger r) => Connection p -> NetworkT r ()
recvAll conn = do
    Handle { hConns
           , hCallbacks = Callbacks {connectionLost, recvPayload}
           } <- view handle

    ok <- io . atomically $ Conn.activeAdd hConns conn
    if ok then
        E.onException (runConduit $ recv conn recvPayload) . io $ do
            atomically $ Conn.activeDel_ hConns conn
            connectionLost $ toPeer conn
    else
        goaway conn "Duplicate Node Id"
  where
    goaway c msg = do
        Log.errM Log.stext msg
        io $ connSendWire c (WireGoaway (pure msg))

    recv c recvPayload =
        transPipe io (connRecvWire c) .| Conduit.mapM_ (\case
            WirePayload p  -> io $ recvPayload (toPeer c) p
            WireGoaway msg -> Log.errM fgoaway "GOAWAY received: " msg
                           *> E.throwM ProtocolError)

    toPeer Connection{connNodeId = peerNodeId, connAddr = peerAddr} = Peer{..}

    fgoaway = Log.stext % mapf (fromMaybe mempty) Log.stext

forkUltimately_ :: IO () -> IO a -> IO ()
forkUltimately_ fin work = void $ forkFinally work (const $ fin)

canNotSupported :: HasCallStack => a
canNotSupported = error "CAN addresses not supported"
