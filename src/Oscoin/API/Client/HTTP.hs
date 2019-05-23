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
import           Oscoin.API.Types
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash (Hash, fromHashed)
import           Oscoin.Crypto.PubKey (PublicKey, Signature)

import           Codec.Serialise
import qualified Data.Text as T
import           Formatting.Buildable (Buildable)
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Web.HttpApiData (toUrlPiece)

createNetworkHttpClient
    :: ( Buildable (Hash c)
       , Serialise (BlockHash c)
       , Serialise (PublicKey c)
       , Serialise (Signature c)
       , MonadIO m
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
    :: ( Buildable (Hash c)
       , Serialise (BlockHash c)
       , Serialise (PublicKey c)
       , Serialise (Signature c)
       , Monad m
       )
    => (Request -> m Response) -> Client c m
httpClientFromRequest httpRequest = Client{..}
    where
        submitTransaction tx =
            post httpRequest "/transactions" tx

        getTransaction txId =
            get httpRequest $ "/transactions/" <> toUrlPiece (fromHashed txId)

        getState key =
            get httpRequest $ "/state?q=[" <> T.intercalate "," key <> "]"


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


executeRequest :: forall a m. (Serialise a, Monad m) => Requester m -> Request -> m (Result a)
executeRequest httpRequest req = do
    Response{..} <- httpRequest req
    pure $ case deserialiseOrFail $ responseBody of
        Left _    -> Err "Failed to deserialise response"
        Right val -> val

get :: (Monad m, Serialise a) => Requester m -> Text -> m (Result a)
get httpRequest reqPath =
    executeRequest httpRequest $
        Request
            { requestMethod = methodGet
            , requestPath = reqPath
            , requestHeaders = [(hAccept, "application/cbor")]
            , requestBody = mempty
            }

post :: (Monad m, Serialise a, Serialise b) => Requester m -> Text -> a -> m (Result b)
post httpRequest reqPath reqBody =
    executeRequest httpRequest $
        Request
            { requestMethod = methodPost
            , requestPath = reqPath
            , requestHeaders = [(hAccept, "application/cbor"), (hContentType, "application/cbor")]
            , requestBody = serialise reqBody
            }
