-- | Implementation of 'MonadClient' using the HTTP API and
-- "Network.Http.Client".
--
-- @
-- foo :: 'MonadClient' m => m a
-- main = 'runHttpClientT' "http://localhost:8080" foo
-- @
--
-- 'runHttpClientT'' allows you to use a custom request function.
module Oscoin.API.HTTP.Client
    ( runHttpClientT
    , HttpClientT
    , MonadClient(..)

    -- * Custom HTTP client
    , runHttpClientT'
    , Request(..)
    , Response(..)
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Client
import           Oscoin.API.Types
import           Oscoin.Crypto.Hash (fromHashed)

import           Codec.Serialise
import qualified Data.Text as T
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Web.HttpApiData (toUrlPiece)


type Requester m = Request -> m Response

data Request = Request
    { requestMethod  :: Method
    , requestPath    :: Text
    , requestHeaders :: RequestHeaders
    , requestBody    :: LByteString
    }

data Response = Response
    { responseBody   :: LByteString
    , responseStatus :: Status
    }

newtype HttpClientT m a = HttpClientT (ReaderT (Requester m) m a)
    deriving (Functor, Applicative, Monad, MonadMask, MonadCatch, MonadThrow, MonadIO)

instance MonadTrans HttpClientT where
    lift ma = HttpClientT $ lift ma

instance (Monad m, MonadIO m) => MonadClient (HttpClientT m) where
    submitTransaction tx =
        post "/transactions" tx

    getTransaction txId =
        get $ "/transactions/" <> toUrlPiece (fromHashed txId)

    getState key =
        get $ "/state?q=[" <> T.intercalate "," key <> "]"


-- | @runHttpClientT uri@ throws an exception if @uri@ is not a valid URI.
runHttpClientT :: (MonadIO m) => Text -> HttpClientT m a -> m a
runHttpClientT uri (HttpClientT m) = do
    client <- newHttpClient uri
    runReaderT m client

runHttpClientT' :: (Request -> m Response) -> HttpClientT m a -> m a
runHttpClientT' mkRequest (HttpClientT m) = runReaderT m mkRequest

------------------------------------------------------------------------------

newHttpClient :: (MonadIO m) => Text -> m (Requester m)
newHttpClient uri = liftIO $ do
    manager <- Client.newManager Client.defaultManagerSettings
    baseRequest <- Client.parseRequest (toS uri)
    pure $ liftIO . makeHttpRequest manager baseRequest

makeHttpRequest :: Client.Manager -> Client.Request -> Request -> IO Response
makeHttpRequest manager baseRequest Request{..} = do
    let req = baseRequest
            { Client.method = requestMethod
            , Client.path = encodeUtf8 requestPath
            , Client.requestHeaders = requestHeaders
            , Client.requestBody = Client.RequestBodyLBS $ requestBody
            }
    res <- Client.httpLbs req manager
    pure $ Response
        { responseStatus = Client.responseStatus res
        , responseBody = Client.responseBody res
        }


makeRequest :: forall a m. (Serialise a, MonadIO m) => Request -> HttpClientT m (Result a)
makeRequest req = HttpClientT $ do
    mkRequest <- ask
    Response{..} <- lift $ mkRequest req
    pure $ case deserialiseOrFail $ responseBody of
        Left _    -> Err "Failed to deserialise response"
        Right val -> val

get :: (MonadIO m, Serialise a) => Text -> HttpClientT m (Result a)
get reqPath =
    makeRequest
        Request
            { requestMethod = methodGet
            , requestPath = reqPath
            , requestHeaders = [(hAccept, "application/cbor")]
            , requestBody = mempty
            }

post :: (MonadIO m, Serialise a, Serialise b) => Text -> a ->  HttpClientT m (Result b)
post reqPath reqBody =
    makeRequest
        Request
            { requestMethod = methodPost
            , requestPath = reqPath
            , requestHeaders = [(hAccept, "application/cbor"), (hContentType, "application/cbor")]
            , requestBody = serialise reqBody
            }
