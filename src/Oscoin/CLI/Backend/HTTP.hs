module Oscoin.CLI.Backend.HTTP where

import           Oscoin.Prelude

import qualified Oscoin.CLI.Backend as Backend
import           Oscoin.CLI.Backend (Backend, Backend(Backend))
import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.Revision
import           Oscoin.CLI.Radicle

import           Oscoin.Data.Tx
import qualified Oscoin.Node as Node
import qualified Oscoin.API.HTTP.Result as API
import           Oscoin.API.HTTP (ApiTx)
import           Oscoin.Crypto.PubKey
import           Oscoin.Crypto.Hash (hash)

import           Codec.Serialise
import           Control.Exception (Exception, throwIO)
import           Crypto.Random.Types (MonadRandom)
import           Data.IP (IP)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import           Network.HTTP.Client
import           Network.HTTP.Types.Header (hAccept, hContentType)

data Handle = Handle
    { httpRemoteIp   :: IP
    , httpRemotePort :: Port
    , httpManager    :: Manager
    }

newtype HttpClientException = HttpClientException Text
    deriving (Show)
instance Exception HttpClientException

type Port = Word16

newtype Path = Path { fromPath :: ByteString }
    deriving (IsString, Eq, Show, Semigroup, Monoid)

data Method = GET | PUT | POST | DELETE
    deriving (Show)

new :: IP -> Port -> IO (Backend Text)
new ip port = do
    h <- Handle ip port <$> newManager defaultManagerSettings
    pure Backend
        { Backend.revisionCreate  = revisionCreate h
        , Backend.revisionList    = revisionList h
        , Backend.revisionStatus  = revisionStatus h
        , Backend.revisionMerge   = revisionMerge h
        , Backend.revisionSuggest = revisionSuggest h
        }

get :: Handle -> Path -> IO (Response LBS.ByteString)
get h reqPath =
    request @() h GET reqPath Nothing

post :: Serialise a => Handle -> Path -> a -> IO (Response LBS.ByteString)
post h reqPath reqBody =
    request h POST reqPath (Just reqBody)

request :: Serialise a => Handle -> Method -> Path -> Maybe a -> IO (Response LBS.ByteString)
request Handle{..} reqMethod reqPath reqBody =
    httpLbs req httpManager
  where
    req = defaultRequest
        { method         = C8.pack (show reqMethod)
        , host           = C8.pack (show httpRemoteIp)
        , port           = fromIntegral httpRemotePort
        , path           = fromPath reqPath
        , requestHeaders = (hAccept, "application/cbor") : contentType reqBody
        , requestBody    = (RequestBodyLBS . maybe LBS.empty serialise) reqBody
        }
    contentType (Just _) = [(hContentType, "application/cbor")]
    contentType _        = []

submitPath :: Path
submitPath = "/node/mempool"

readPath :: Path
readPath = "/node/state"

submitTransaction :: Handle -> ApiTx -> IO (API.Result (Node.Receipt a))
submitTransaction h tx = do
    resp <- post h submitPath tx
    if LBS.null (responseBody resp)
       then throwIO (HttpClientException "Oscoin.CLI.Backend.HTTP: Empty body")
       else case deserialiseOrFail (responseBody resp) of
           Left err  -> throwIO (HttpClientException $ "Oscoin.CLI.Backend.HTTP: Failed to deserialise body: " <> tshow err)
           Right val -> pure val

createRevisionTx
    :: MonadRandom m => Revision -> (PublicKey, PrivateKey) -> m ApiTx
createRevisionTx rev (pk, sk) = do
    msg <- sign sk (toRadicle rev)
    pure $ mkTx msg (hash pk)

--------------------------------------------------------------------------------

revisionCreate :: Handle -> Revision -> (PublicKey, PrivateKey) -> IO (Result Text)
revisionCreate h rev kp = do
    result <- submitTransaction h =<< createRevisionTx rev kp
    pure $ case result of
        API.Ok v    -> ResultValue (tshow v)
        API.Err err -> ResultError err

revisionStatus :: FromRadicle a => Handle -> RevisionId -> IO (Result a)
revisionStatus h (RevisionId id) = do
    resp <- get h (readPath <> "/revisions/" <> fromString (show id))
    if LBS.null (responseBody resp)
       then throwIO (HttpClientException "Oscoin.CLI.Backend.HTTP: Empty body")
       else case deserialiseOrFail (responseBody resp) of
           Left err          -> throwIO (HttpClientException $ "Oscoin.CLI.Backend.HTTP: Failed to deserialise body: " <> tshow err)
           Right (API.Ok v)  -> pure $ ResultValue $ fromRadicle v
           Right (API.Err e) -> pure $ ResultError e

revisionMerge :: Handle -> RevisionId -> IO (Result a)
revisionMerge _ _ = pure ResultOk

revisionSuggest :: Handle -> RevisionId -> Suggestion -> IO (Result a)
revisionSuggest _ _ _ = pure ResultOk

revisionList :: Handle -> IO (Result a)
revisionList _ = pure ResultOk

