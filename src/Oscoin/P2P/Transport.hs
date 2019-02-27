module Oscoin.P2P.Transport
    ( RecvError(..)

    -- * Framed
    , Framed
    , framed
    , anyFramed
    , framedSend
    , framedRecv
    , framedEnvelope

    -- * Streaming
    , Streaming
    , streaming
    , anyStreaming
    , streamingSend
    , streamingRecv
    , streamingEnvelope
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR

import           Data.Conduit (ConduitT, catchC, (.|))
import qualified Data.Conduit.Combinators as Conduit
import           Data.Conduit.Network (sourceSocket)
import           Data.Conduit.Serialise (conduitDecodeCBOR)

import           Control.Monad.ST (RealWorld, ST, stToIO)
import           Data.Bitraversable (bitraverse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Network.Socket (Socket)
import qualified Network.Socket.ByteString as Sock

import           System.Timeout (timeout)

data RecvError =
      RecvTimeout
    | RecvGarbage CBOR.DeserialiseFailure
    | RecvConnReset
    deriving (Eq, Show)

instance Exception RecvError

-- Framed ----------------------------------------------------------------------

data Framed = Framed
    { fSend :: ByteString -> IO ()
    , fRecv :: IO (Either RecvError ByteString)
    }

framed :: Socket -> Framed
framed sock = Framed {..}
  where
    fSend bs =
        let len = CBOR.serialise $ BS.length bs
         in Sock.sendMany sock [LBS.toStrict len, bs]

    fRecv = do
        len <- recvIncr (recv sock 8) =<< stToIO CBOR.deserialiseIncremental
        case len of
            Left  (e, _)    -> pure $ Left e
            Right (l, more) -> map (more <>) <$> recv sock (l - BS.length more)

anyFramed :: (ByteString -> IO ()) -> IO (Either RecvError ByteString) -> Framed
anyFramed = Framed

framedSend :: Serialise a => Framed -> a -> IO ()
framedSend (fSend -> sendBytes) a = sendBytes . LBS.toStrict $ CBOR.serialise a

framedRecv :: Serialise a => Framed -> IO (Either RecvError a)
framedRecv (fRecv -> recvBytes) = do
    bytes <- recvBytes
    pure $ bytes >>= first RecvGarbage . CBOR.deserialiseOrFail . LBS.fromStrict

framedEnvelope
    :: Serialise a
    => (ByteString -> IO a)
    -> (a -> IO ByteString)
    -> Framed
    -> Framed
framedEnvelope s r f = Framed {..}
  where
    fSend = s >=> framedSend f
    fRecv = framedRecv f >>= bitraverse pure r

-- Streaming -------------------------------------------------------------------

-- TODO(kim): make this untyped as well

data Streaming wire = Streaming
    { sSend :: wire -> IO ()
    , sRecv :: ConduitT () wire IO ()
    }

streaming :: Serialise wire => Socket -> Streaming wire
streaming sock = Streaming {..}
  where
    sSend = Sock.sendMany sock . LBS.toChunks . CBOR.serialise
    sRecv = sourceSocket sock .| catchC conduitDecodeCBOR (throwM . RecvGarbage)

anyStreaming :: (wire -> IO ()) -> ConduitT () wire IO () -> Streaming wire
anyStreaming = Streaming

streamingSend :: Streaming a -> a -> IO ()
streamingSend = sSend

streamingRecv :: Streaming a -> ConduitT () a IO ()
streamingRecv = sRecv

streamingEnvelope
    :: (wire  -> IO wire')
    -> (wire' -> IO wire)
    -> Streaming wire'
    -> Streaming wire
streamingEnvelope s r t = Streaming {..}
  where
    sSend = s >=> streamingSend t
    sRecv = streamingRecv t .| Conduit.mapM r

--------------------------------------------------------------------------------

recv :: Socket -> Int -> IO (Either RecvError ByteString)
recv sock n | n < 1     = pure $ Right mempty
            | otherwise = do
    bytes <- timeout 500000 $ Sock.recv sock n
    pure $ case bytes of
        Nothing -> Left RecvTimeout
        Just bs | BS.null bs -> Left RecvConnReset
                | otherwise  -> Right bs

recvIncr
    :: MonadIO m
    => m (Either RecvError ByteString)
    -> CBOR.IDecode RealWorld a
    -> m (Either (RecvError, ByteString)
                 (a,         ByteString))
recvIncr _     (CBOR.Done l _ a) = pure $ Right (a, l)
recvIncr _     (CBOR.Fail l _ e) = pure $ Left  (RecvGarbage e, l)
recvIncr recv' part              = recv' >>= \case
    Left  e  -> pure $ Left (e, mempty)
    Right bs -> liftIO (stToIO $ feed bs part) >>= recvIncr (pure (Right mempty))

feed :: ByteString -> CBOR.IDecode s a -> ST s (CBOR.IDecode s a)
feed bs (CBOR.Done l o a) = pure $ CBOR.Done (l <> bs) o a
feed bs (CBOR.Fail l o e) = pure $ CBOR.Fail (l <> bs) o e
feed bs (CBOR.Partial  k) = k (Just bs)
