{-# LANGUAGE TupleSections #-}

module Oscoin.P2P.Connection
    ( Connection
    , connNodeId
    , connAddr
    , connSendWire
    , connRecvWire
    , connClose
    , mkConnection

    , Active
    , activeNew
    , activeAdd
    , activeDel
    , activeDel_
    , activeGet
    , activeHas

    , RecvError (..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.P2P.Transport as Transport
import           Oscoin.P2P.Types (NodeId)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import           Data.Conduit (ConduitT)
import qualified Focus
import           Network.Socket (SockAddr, Socket)
import qualified Network.Socket as Sock
import qualified STMContainers.Map as STMMap

-- | Captures the identity of a mutual peer connection, and handling of the
-- underlying transport.
data Connection wire = Connection
    { connNodeId   :: NodeId
    , connAddr     :: SockAddr
    , connSendWire :: wire -> IO ()
    , connRecvWire :: ConduitT () wire IO ()
    , connClose    :: IO ()
    }

mkConnection
    :: Serialise wire'
    => NodeId
    -> Socket
    -> SockAddr
    -> (wire  -> IO wire')
    -> (wire' -> IO wire )
    -> IO (Connection wire)
mkConnection nid sock addr tsend trecv = do
    mutex <- newMVar ()
    pure Connection
        { connNodeId   = nid
        , connAddr     = addr
        , connSendWire = withMVar mutex . const . Transport.streamingSend transport
        , connRecvWire = Transport.streamingRecv transport
        , connClose    = Sock.close sock
        }
  where
    transport =
        Transport.streamingEnvelope tsend trecv $ Transport.streaming sock

newtype Active p = Active (STMMap.Map NodeId (Connection p))

data RecvError =
      RecvTimeout
    | RecvGarbage CBOR.DeserialiseFailure
    | RecvConnReset
    deriving Show

instance Exception RecvError

activeNew :: STM (Active p)
activeNew = Active <$> STMMap.new

activeAdd :: Active p -> Connection p -> STM Bool
activeAdd (Active actv) conn = do
    prev <- STMMap.lookup nid actv
    case prev of
        Nothing -> STMMap.insert conn nid actv $> True
        Just _  -> pure False
  where
    nid = connNodeId conn

activeDel_ :: Active p -> Connection p -> STM ()
activeDel_ (Active actv) conn = STMMap.delete (connNodeId conn) actv

activeDel :: Active p -> NodeId -> STM (Maybe (Connection p))
activeDel (Active actv) nid = STMMap.focus (pure . (,Focus.Remove)) nid actv

activeGet :: Active p -> NodeId -> STM (Maybe (Connection p))
activeGet (Active actv) nid = STMMap.lookup nid actv

activeHas :: Active p -> NodeId -> STM Bool
activeHas (Active actv) nid =
    STMMap.focus (pure . (,Focus.Keep) . isJust) nid actv
