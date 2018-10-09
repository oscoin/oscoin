{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Oscoin.P2P.Connection
    ( Connection
    , connNodeId
    , connAddr
    , connSendWire
    , connRecvWire
    , connClose
    , mkConnection

    , Socket'
    , framedSend
    , framedRecv
    , mkSocket'

    , Active
    , activeNew
    , activeAdd
    , activeDel
    , activeDel_
    , activeGet
    , activeHas

    , RecvError (..)

    , sockSendFramed
    , sockRecvFramed

    , sockSendStream
    , sockRecvStream

    -- * Conduits
    , sourceSocketLBS
    ) where

import           Oscoin.Prelude

import           Oscoin.P2P.Types (NodeId)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import           Control.Exception.Safe (Exception, MonadThrow, throwM)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.ST (RealWorld, ST, stToIO)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Conduit (ConduitT, catchC, yield, (.|))
import qualified Data.Conduit.Combinators as Conduit
import           Data.Conduit.Serialise (conduitDecodeCBOR)
import           Data.Int (Int64)
import qualified Focus
import           Network.Socket (SockAddr, Socket)
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockBS
import qualified Network.Socket.ByteString.Lazy as SockLBS
import qualified STMContainers.Map as STMMap
import           System.Timeout (timeout)

newtype Socket' = Socket' Socket

mkSocket' :: Socket -> Socket'
mkSocket' = Socket'

framedSend :: Serialise p => Socket' -> p -> IO ()
framedSend (Socket' sock) = sockSendFramed sock

framedRecv :: Serialise p => Socket' -> IO (Either RecvError p)
framedRecv (Socket' sock) = sockRecvFramed sock

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
        , connSendWire = tsend >=> sendWire mutex
        , connRecvWire = recvWire
        , connClose    = Sock.close sock
        }
  where
    sendWire mutex wire =
        withMVar mutex . const $
            sockSendStream sock wire

    recvWire =
           sockRecvStream sock
        .| conduitDecodeCBOR
        .| Conduit.mapM trecv

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

sockSendFramed :: Serialise a => Socket -> a -> IO ()
sockSendFramed sock x =
    SockBS.sendMany sock $ LBS.toStrict len : LBS.toChunks bytes
  where
    bytes = CBOR.serialise x
    len   = CBOR.serialise $ LBS.length bytes

sockRecvFramed :: Serialise a => Socket -> IO (Either RecvError a)
sockRecvFramed sock = do
    len <- recvIncr (recv sock 8) =<< stToIO CBOR.deserialiseIncremental
    case len of
        Left  (e, _)    -> pure $ Left e
        Right (l, more) -> do
            bytes <- map (mappend more) <$> recv sock (l - LBS.length more)
            pure $ bytes >>= first RecvGarbage . CBOR.deserialiseOrFail

sockSendStream :: Serialise a => Socket -> a -> IO ()
sockSendStream sock x = SockBS.sendMany sock . LBS.toChunks $ CBOR.serialise x

sockRecvStream
    :: ( MonadUnliftIO m
       , MonadThrow    m
       , Serialise a
       )
    => Socket
    -> ConduitT i a m ()
sockRecvStream sock =
    sourceSocketLBS sock .| catchC conduitDecodeCBOR (throwM . RecvGarbage)

sourceSocketLBS
    :: (MonadIO m, MonadThrow m)
    => Socket
    -> ConduitT i LBS.ByteString m ()
sourceSocketLBS sock = loop
  where
    loop = lift (liftIO $ recv sock 4096) >>= \case
        Left  RecvConnReset -> yield mempty
        Left  e             -> throwM e
        Right bs            -> yield bs *> loop

--------------------------------------------------------------------------------

recv :: Socket -> Int64 -> IO (Either RecvError LBS.ByteString)
recv sock n | n < 1     = pure $ Right mempty
            | otherwise = do
    bytes <- timeout 500000 $ SockLBS.recv sock n
    pure $ case bytes of
        Nothing -> Left RecvTimeout
        Just bs | LBS.null bs -> Left RecvConnReset
                | otherwise   -> Right bs

recvIncr
    :: MonadIO m
    => m (Either RecvError LBS.ByteString)
    -> CBOR.IDecode RealWorld a
    -> m (Either (RecvError, LBS.ByteString)
                 (a,         LBS.ByteString))
recvIncr _     (CBOR.Done l _ a) = pure $ Right (a, LBS.fromStrict l)
recvIncr _     (CBOR.Fail l _ e) = pure $ Left  (RecvGarbage e, LBS.fromStrict l)
recvIncr recv' part              = recv' >>= \case
    Left  e  -> pure $ Left (e, mempty)
    Right bs -> liftIO (stToIO $ feed bs part) >>= recvIncr (pure (Right mempty))

feed :: LBS.ByteString -> CBOR.IDecode s a -> ST s (CBOR.IDecode s a)
feed lbs (CBOR.Done l o a) = pure $ CBOR.Done (l <> LBS.toStrict lbs) o a
feed lbs (CBOR.Fail l o e) = pure $ CBOR.Fail (l <> LBS.toStrict lbs) o e
feed lbs (CBOR.Partial  k) = case lbs of
    LBS.Chunk chunk more -> k (Just chunk) >>= feed more
    LBS.Empty            -> k Nothing      >>= feed LBS.Empty
