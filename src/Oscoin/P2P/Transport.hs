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

import           Data.Conduit (ConduitT, catchC, yield, (.|))
import qualified Data.Conduit.Combinators as Conduit
import           Data.Conduit.Serialise (conduitDecodeCBOR)

import           Control.Monad.ST (RealWorld, ST, stToIO)
import           Data.Bitraversable (bitraverse)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS

import           Network.Socket (Socket)
import qualified Network.Socket.ByteString as SockBS
import qualified Network.Socket.ByteString.Lazy as SockLBS

import           System.Timeout (timeout)

data RecvError =
      RecvTimeout
    | RecvGarbage CBOR.DeserialiseFailure
    | RecvConnReset
    deriving (Eq, Show)

instance Exception RecvError

-- Framed ----------------------------------------------------------------------

data Framed = Framed
    { fSend :: LByteString -> IO ()
    , fRecv :: IO (Either RecvError LByteString)
    }

framed :: Socket -> Framed
framed sock = Framed {..}
  where
    fSend bs =
        let len = CBOR.serialise $ LBS.length bs
         in SockBS.sendMany sock $ LBS.toStrict len : LBS.toChunks bs

    fRecv = do
        len <- recvIncr (recv sock 8) =<< stToIO CBOR.deserialiseIncremental
        case len of
            Left  (e, _)    -> pure $ Left e
            Right (l, more) -> map (mappend more) <$> recv sock (l - LBS.length more)

anyFramed
    :: (LByteString -> IO ())
    -> IO (Either RecvError LByteString)
    -> Framed
anyFramed = Framed

framedSend :: Serialise a => Framed -> a -> IO ()
framedSend (fSend -> sendBytes) a = sendBytes $ CBOR.serialise a

framedRecv :: Serialise a => Framed -> IO (Either RecvError a)
framedRecv (fRecv -> recvBytes) = do
    bytes <- recvBytes
    pure $ bytes >>= first RecvGarbage . CBOR.deserialiseOrFail

framedEnvelope
    :: Serialise a
    => (LByteString -> IO a)
    -> (a -> IO LByteString)
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
    sSend = SockBS.sendMany sock . LBS.toChunks . CBOR.serialise
    sRecv = sourceSocketLBS sock .| catchC conduitDecodeCBOR (throwM . RecvGarbage)

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

sourceSocketLBS :: Socket -> ConduitT i LByteString IO ()
sourceSocketLBS sock = loop
  where
    loop = lift (recv sock 4096) >>= \case
        Left  RecvConnReset -> yield mempty
        Left  e             -> throwM e
        Right bs            -> yield bs *> loop

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
