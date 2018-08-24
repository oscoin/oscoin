module Oscoin.CLI.Backend.HTTP where

import           Oscoin.Prelude

import qualified Oscoin.CLI.Backend as Backend
import           Oscoin.CLI.Backend (Backend, Backend(Backend))
import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.Revision
import           Oscoin.CLI.Radicle

import           Codec.Serialise
import           Control.Exception (Exception, throwIO)
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

get :: Serialise a => Handle -> Path -> IO a
get h reqPath =
    request @() h GET reqPath Nothing

post :: (Serialise a, Serialise b) => Handle -> Path -> b -> IO a
post h reqPath reqBody =
    request h POST reqPath (Just reqBody)

request :: (Serialise a, Serialise b) => Handle -> Method -> Path -> Maybe a -> IO b
request Handle{..} reqMethod reqPath reqBody = do
    res <- httpLbs req httpManager
    case deserialise (responseBody res) of
        Just json -> pure json
        Nothing   -> throwIO (HttpClientException "Error parsing response body")
  where
    req = defaultRequest
        { method         = C8.pack (show reqMethod)
        , host           = C8.pack (show httpRemoteIp)
        , port           = fromIntegral httpRemotePort
        , path           = fromPath reqPath
        , requestHeaders = (hAccept, "application/cbor") : contentType reqBody
        , requestBody    = maybe (RequestBodyLBS LBS.empty)
                                 (RequestBodyLBS . serialise)
                                 reqBody
        }
    contentType (Just _) = [(hContentType, "application/cbor")]
    contentType _        = []

evalPath :: Path
evalPath = "/radicle/eval"

readPath :: Path
readPath = "/radicle/state"

--------------------------------------------------------------------------------

revisionCreate :: FromRadicle a => Handle -> Revision -> IO (Result a)
revisionCreate h rev =
    fromRadicle <$> post h evalPath (toRadicle rev)

revisionStatus :: FromRadicle a => Handle -> RevisionId -> IO (Result a)
revisionStatus h (RevisionId id) =
    fromRadicle <$> get h (readPath <> "/revisions/" <> fromString (show id))

revisionMerge :: Handle -> RevisionId -> IO (Result a)
revisionMerge _ _ = pure ResultOk

revisionSuggest :: Handle -> RevisionId -> Suggestion -> IO (Result a)
revisionSuggest _ _ _ = pure ResultOk

revisionList :: Handle -> IO (Result a)
revisionList _ = pure ResultOk

