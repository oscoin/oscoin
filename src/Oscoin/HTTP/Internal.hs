module Oscoin.HTTP.Internal where

import           Oscoin.Prelude

import           Oscoin.Environment
import qualified Oscoin.Node as Node
import           Oscoin.Data.Tx (Tx)

import qualified Radicle as Rad

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (listToMaybe)
import           Data.List (intersect, lookup)
import           Data.Text.Encoding (decodeUtf8')
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
import           Network.HTTP.Types.Header (HeaderName)
import qualified Network.Wai as Wai
import           Network.Wai.Parse (parseHttpAccept, parseContentType)
import qualified Network.Wai.Middleware.RequestLogger as Wai
import           Web.HttpApiData (FromHttpApiData)
import           Web.Spock (HasSpock, SpockAction, SpockConn, SpockM, runSpock, spock)
import qualified Web.Spock as Spock
import           Web.Spock.Config (ConnBuilder(..), PoolCfg(..), PoolOrConn(..), defaultSpockCfg)

-- | The global server state.
data State = State ()
    deriving (Show)

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction s i = SpockAction (Node.Handle ApiTx s i) () State

-- | The type of a block transaction in the API.
type ApiTx = Tx Rad.Value

instance MonadFail (ApiAction s i) where
    fail = error "failing!"

-- | The type of our api.
type Api s i = SpockM (Node.Handle ApiTx s i) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi s i m = (HasSpock m, SpockConn m ~ Node.Handle ApiTx s i)

-- | Create an empty state.
mkState :: State
mkState = State ()

getHeader :: HeaderName -> ApiAction s i (Maybe Text)
getHeader name = do
    header <- Spock.rawHeader name
    pure $ header >>= (rightToMaybe . decodeUtf8')

getHeader' :: HeaderName -> ApiAction s i Text
getHeader' name = do
    header <- getHeader name
    case header of
        Just v  -> pure v
        Nothing -> respond HTTP.badRequest400 noBody

getRawBody :: ApiAction s i LBS.ByteString
getRawBody = LBS.fromStrict <$> Spock.body

-- | A sum type of supported content types.
data ContentType = JSON | CBOR deriving (Ord, Eq, Show)

fromContentType :: ContentType -> Text
fromContentType JSON = "application/json"
fromContentType CBOR = "application/cbor"

supportedContentTypes :: NonEmpty (Text, ContentType)
supportedContentTypes = NonEmpty.fromList $ [(fromContentType ct, ct) | ct <- [JSON, CBOR]]

-- | Gets the Accept header, defaulting to application/json if not present.
getAccept :: ApiAction s i Text
getAccept = maybe ((fst . NonEmpty.head) supportedContentTypes) identity <$> getHeader "Accept"

-- | Gets the parsed content types out of the Accept header, ordered by priority.
getAccepted :: ApiAction s i [Text]
getAccepted = do
    accept <- getAccept
    let accepted = decodeUtf8' <$> parseHttpAccept (encodeUtf8 accept)
    case lefts accepted of
        [] -> pure $ rights $ accepted
        _  -> respond HTTP.badRequest400 noBody

-- | Returns Just the first `accepted` content type also present in `offered`
-- or Nothing if no offers match the accepted types.
bestContentType :: [Text] -> [Text] -> Maybe Text
bestContentType accepted offered = listToMaybe $ accepted `intersect` offered

-- | Negotiates the best response content type from the request's accept header.
negotiateContentType :: ApiAction s i ContentType
negotiateContentType = do
    accepted <- getAccepted
    let supported = NonEmpty.toList supportedContentTypes
    let offered = fst <$> supported
    case bestContentType accepted offered of
        Nothing -> respond HTTP.notAcceptable406 noBody
        Just ct -> pure $ fromJust $ lookup ct supported

getState :: ApiAction s i State
getState = Spock.getState

param' :: (FromHttpApiData p) => Text -> ApiAction s i p
param' = Spock.param'

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = Spock.runQuery

body :: a -> Maybe a
body = Just

noBody :: Maybe ()
noBody = Nothing

encode :: (Serialise a, ToJSON a) => ContentType -> a -> LBS.ByteString
encode JSON = Aeson.encode
encode CBOR = Serialise.serialise

decode :: (Serialise a, FromJSON a) => ContentType -> LBS.ByteString -> Either Text a
decode JSON bs = first T.pack          (Aeson.eitherDecode' bs)
decode CBOR bs = first (T.pack . show) (Serialise.deserialiseOrFail bs)

respond :: (ToJSON a, Serialise a) => HTTP.Status -> Maybe a -> ApiAction s i b
respond status (Just bdy) = do
    ct <- negotiateContentType
    respondBytes status ct (encode ct bdy)
respond status Nothing = do
    Spock.setStatus status
    Spock.lazyBytes ""

respondBytes :: HTTP.Status -> ContentType -> LBS.ByteString -> ApiAction s i b
respondBytes status ct bdy = do
    Spock.setStatus status
    Spock.setHeader "Content-Type" $ fromContentType ct
    Spock.lazyBytes bdy

getContentType :: ApiAction s i Text
getContentType = do
    ctype <- getHeader' "Content-Type"
    case decodeUtf8' $ fst $ parseContentType $ encodeUtf8 ctype of
        Left  _  -> respond HTTP.unsupportedMediaType415 noBody
        Right ct -> pure ct

getSupportedContentType :: ApiAction s i ContentType
getSupportedContentType = do
    ct <- getContentType
    let supported = NonEmpty.toList supportedContentTypes
    case lookup ct supported of
        Nothing  -> respond HTTP.unsupportedMediaType415 noBody
        Just sct -> pure sct

getBody :: (Serialise a, FromJSON a) => ApiAction s i a
getBody = do
    ct <- getSupportedContentType
    !body' <- getRawBody
    case decode ct body' of
        Left  _ -> respond HTTP.badRequest400 noBody
        Right a -> pure a

notImplemented :: ApiAction s i ()
notImplemented = respond HTTP.notImplemented501 noBody

run :: Api s i ()
    -> Int
    -> Node.Handle ApiTx s i
    -> IO ()
run app port hdl =
    runSpock port (mkMiddleware app hdl)

mkMiddleware
    :: Api s i ()
    -> Node.Handle ApiTx s i
    -> IO Wai.Middleware
mkMiddleware app hdl = do
    spockCfg <- defaultSpockCfg () (PCConn connBuilder) state
    spock spockCfg app
  where
    connBuilder = ConnBuilder (pure hdl) (const pass) (PoolCfg 1 1 30)
    state       = mkState

loggingMiddleware :: Environment -> Wai.Middleware
loggingMiddleware Production = Wai.logStdout
loggingMiddleware Development = Wai.logStdoutDev
loggingMiddleware Testing = identity
