{-# LANGUAGE UndecidableInstances #-}
-- | Provides 'createNetworkHttpClient' to create a 'Client'
-- implementation using "Network.HTTP.Client".
--
-- Also provides 'makeHttpClient' to build a 'Client' using just an
-- HTTP request function.
module Oscoin.API.Client.HTTP
    ( createNetworkHttpClient
    , httpClientFromRequest
    , Request(..)
    , Response(..)
    ) where

import           Oscoin.Prelude hiding (get)

import           Oscoin.API.Client
import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Crypto.PubKey (PublicKey, Signature)

import           Codec.Serialise
import qualified Data.ByteString.BaseN as BaseN
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status

createNetworkHttpClient
    :: ( Serialise (Hash c)
       , Serialise (PublicKey c)
       , Serialise (Signature c)
       , MonadIO m
       , MonadThrow m
       , MonadIO n
       )
    => Text -> n (Client c m)
createNetworkHttpClient uri = liftIO $ do
    manager <- Client.newManager Client.defaultManagerSettings
    baseRequest <- Client.parseRequest (toS uri)
    let httpRequest = liftIO . makeHttpRequest manager baseRequest
    pure $ httpClientFromRequest httpRequest

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


-- | Make a 'Client' from a function executing the underlying HTTP
-- requests.
httpClientFromRequest
    :: ( Serialise (Hash c)
       , Serialise (PublicKey c)
       , Serialise (Signature c)
       , MonadThrow m
       )
    => (Request -> m Response) -> Client c m
httpClientFromRequest httpRequest = Client{..}
    where
        submitTransaction tx =
            post httpRequest "/transactions" tx

        getState key =
            get httpRequest $ "/state/" <> toS (BaseN.encodedBytes $ BaseN.encodeBase16 key)


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


executeRequest
    :: (Serialise a, MonadThrow m)
    => Requester m -> Request -> m a
executeRequest httpRequest req = do
    Response{..} <- httpRequest req
    case deserialiseOrFail $ responseBody of
        Left err  -> throw err
        Right val -> pure val

get
    :: (MonadThrow m, Serialise a)
    => Requester m -> Text -> m a
get httpRequest reqPath =
    executeRequest httpRequest $
        Request
            { requestMethod = methodGet
            , requestPath = reqPath
            , requestHeaders = [(hAccept, "application/cbor")]
            , requestBody = mempty
            }

post
    :: (MonadThrow m, Serialise a, Serialise b)
    => Requester m -> Text -> a -> m b
post httpRequest reqPath reqBody =
    executeRequest httpRequest $
        Request
            { requestMethod = methodPost
            , requestPath = reqPath
            , requestHeaders = [(hAccept, "application/cbor"), (hContentType, "application/cbor")]
            , requestBody = serialise reqBody
            }
