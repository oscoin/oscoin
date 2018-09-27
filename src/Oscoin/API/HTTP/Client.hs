{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of 'MonadClient' using the HTTP API.
-- @
--      foo :: MonadClient m => m a
--
--      main = runHttpClient "http://localhost:8080" foo
-- @
module Oscoin.API.HTTP.Client
    ( MonadClient(..)
    , HttpClientT
    , runHttpClientT
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Client
import           Oscoin.API.Types
import           Oscoin.Crypto.Hash (toHexText)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS

import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method


data HttpClient = HttpClient
    { httpClientBaseRequest :: Request
    , httpClientManager     :: Manager
    }


newtype HttpClientT m a = HttpClientT (ReaderT HttpClient m a)
    deriving (Functor, Applicative, Monad, MonadMask, MonadCatch, MonadThrow, MonadTrans, MonadIO)


instance (Monad m, MonadIO m) => MonadClient (HttpClientT m) where
    submitTransaction tx =
        post "/transactions" tx

    getTransaction txId =
        get $ "/transactions/" <> toHexText txId

    getState key =
        get $ "/state/" <> key

type Uri = String

-- | @runHttpClientT uri@ throws an exception if @uri@ is not a valid URI.
runHttpClientT :: (MonadIO m) => Uri -> HttpClientT m a -> m a
runHttpClientT uri (HttpClientT m) = do
    client <- liftIO $ createHttpClient uri
    runReaderT m client


createHttpClient :: Uri -> IO HttpClient
createHttpClient uri = do
    manager <- newManager defaultManagerSettings
    baseRequest <- parseRequest uri
    pure HttpClient { httpClientBaseRequest = baseRequest
                    , httpClientManager = manager
                    }


makeRequest :: (MonadIO m, Serialise a) => (Request -> Request) -> HttpClientT m (Result a)
makeRequest buildRequest = HttpClientT $ do
    HttpClient{..} <- ask
    let req = buildRequest httpClientBaseRequest
    deserialiseResponse <$> liftIO (httpLbs req httpClientManager)

get :: (MonadIO m, Serialise a) => Text -> HttpClientT m (Result a)
get reqPath =
    makeRequest $ \req ->
        req { method = methodGet
            , path = encodeUtf8 reqPath
            , requestHeaders = [(hAccept, "application/cbor")]
            }

post :: (MonadIO m, Serialise a, Serialise b) => Text -> a ->  HttpClientT m (Result b)
post reqPath reqBody =
    makeRequest $ \req ->
        req { method = methodPost
            , path = encodeUtf8 reqPath
            , requestHeaders = [(hAccept, "application/cbor"), (hContentType, "application/cbor")]
            , requestBody = RequestBodyLBS $ serialise reqBody
            }

deserialiseResponse :: (Serialise a) => Response LBS.ByteString -> Result a
deserialiseResponse response =
    case deserialiseOrFail $ responseBody response of
        Left _    -> Err "Failed to deserialise response"
        Right val -> val
