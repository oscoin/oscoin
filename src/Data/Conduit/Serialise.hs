module Data.Conduit.Serialise
    ( conduitEncodeCBOR
    , conduitDecodeCBOR
    )
where

import           Prelude

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import           Control.Exception.Safe (MonadThrow, throwM)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.ST (ST, stToIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Conduit (ConduitT, await, awaitForever, yield)


conduitEncodeCBOR :: (Monad m, Serialise a) => ConduitT a LBS.ByteString m ()
conduitEncodeCBOR = awaitForever $ yield . CBOR.serialise

conduitDecodeCBOR
    :: ( MonadIO    m
       , MonadThrow m
       , Serialise  a
       )
    => ConduitT LBS.ByteString a m ()
conduitDecodeCBOR = sink =<< newDecoder
  where
    sink dec = await >>= maybe (close dec) (push dec)

    push dec bs | LBS.null bs = sink dec
                | otherwise   = go False dec bs

    close dec = go True dec mempty

    go done (CBOR.Done l _ a) bs = do
        yield a
        let leftover = LBS.fromStrict l <> bs
        if done then
            unless (LBS.null leftover) $ do
                dec <- newDecoder
                go done dec leftover
        else
            newDecoder >>= flip push leftover

    go _ (CBOR.Fail _ _ e) _  = throwM e
    go _ partial           bs = lifted (feed bs partial) >>= sink

    newDecoder = lifted CBOR.deserialiseIncremental

    lifted = lift . liftIO . stToIO

feed :: LBS.ByteString -> CBOR.IDecode s a -> ST s (CBOR.IDecode s a)
feed lbs (CBOR.Done l o a) = pure $ CBOR.Done (l <> LBS.toStrict lbs) o a
feed lbs (CBOR.Fail l o e) = pure $ CBOR.Fail (l <> LBS.toStrict lbs) o e
feed lbs (CBOR.Partial  k) = case lbs of
    LBS.Chunk chunk more -> k (Just chunk) >>= feed more
    LBS.Empty            -> k Nothing      >>= feed LBS.Empty
