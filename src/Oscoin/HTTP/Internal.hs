module Oscoin.HTTP.Internal where

import           Oscoin.Prelude

import           Oscoin.Environment
import qualified Oscoin.Node as Node
import           Oscoin.Data.Tx (Tx)

import           Codec.Serialise (Serialise, serialise)
import qualified Codec.Serialise as Serialise
import           Data.Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (listToMaybe)
import           Data.List (intersect, elem)
import           Data.Text.Encoding (decodeUtf8')
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
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
type ApiTx = Tx ByteString

instance MonadFail (ApiAction s i) where
    fail = error "failing!"

-- | The type of our api.
type Api s i = SpockM (Node.Handle ApiTx s i) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi s i m = (HasSpock m, SpockConn m ~ Node.Handle ApiTx s i)

-- | Create an empty state.
mkState :: State
mkState = State ()

getHeader :: Text -> ApiAction s i (Maybe Text)
getHeader = Spock.header

getHeader' :: Text -> ApiAction s i Text
getHeader' h = do
    mh <- getHeader h
    case mh of
        Just v  -> pure v
        Nothing -> respond HTTP.badRequest400

getRawBody :: ApiAction s i LBS.ByteString
getRawBody = LBS.fromStrict <$> Spock.body

-- | Supported content types.
contentTypes :: [Text]
contentTypes = ["application/json", "application/cbor"]

-- | Gets the Accept header, defaulting to application/json if not present.
getAccept :: ApiAction s i Text
getAccept = maybe "application/json" identity <$> getHeader "Accept"

-- | Gets the parsed content types out of the Accept header, ordered by priority.
getAccepted :: ApiAction s i [Text]
getAccepted = do
    accept <- getAccept
    let accepted = decodeUtf8' <$> parseHttpAccept (encodeUtf8 accept)
    case lefts accepted of
        [] -> pure $ rights $ accepted
        _  -> respond HTTP.badRequest400

-- | Returns the Just the first `accepted` content type also present in `offered`
-- or Nothing if no offers match the accepted types.
bestContentType :: [Text] -> [Text] -> Maybe Text
bestContentType accepted offered = listToMaybe $ accepted `intersect` offered

-- | Negotiates the best response content type from the request's accept header.
negotiateContentType :: [Text] -> ApiAction s i Text
negotiateContentType offered = do
    accepted <- getAccepted
    case bestContentType accepted offered of
        Nothing -> respond HTTP.notAcceptable406
        Just ct -> pure ct

getState :: ApiAction s i State
getState = Spock.getState

param' :: (FromHttpApiData p) => Text -> ApiAction s i p
param' = Spock.param'

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = Spock.runQuery

respondJson :: ToJSON a => HTTP.Status -> a -> ApiAction s i ()
respondJson status body = do
    Spock.setHeader "Content-Type" "application/json"
    respondBytes status (Aeson.encode body)

respond :: HTTP.Status -> ApiAction s i a
respond status = respondBytes status mempty

respondCbor :: Serialise a => HTTP.Status -> a -> ApiAction s i b
respondCbor status body = do
    Spock.setHeader "Content-Type" "application/cbor"
    respondBytes status (serialise body)

respondBytes :: HTTP.Status -> LBS.ByteString -> ApiAction s i a
respondBytes status body = do
    Spock.setStatus status
    Spock.lazyBytes body

respondBody :: (ToJSON a, Serialise a) => a -> ApiAction s i ()
respondBody a = do
    ct <- negotiateContentType contentTypes
    Spock.setHeader "Content-Type" ct
    case encode ct a of
        Nothing -> respond HTTP.notAcceptable406
        Just bs -> respondBytes HTTP.accepted202 bs
  where
    encode :: (Serialise a, ToJSON a) => Text -> a -> Maybe LBS.ByteString
    encode "application/json" = Just . Aeson.encode
    encode "application/cbor" = Just . Serialise.serialise
    encode _                  = const Nothing

getContentType :: ApiAction s i Text
getContentType = do
    ctype <- getHeader "Content-Type"
    case ctype of
        Nothing -> respond HTTP.unsupportedMediaType415
        Just ct -> case decodeUtf8' $ fst $ parseContentType $ encodeUtf8 ct of
            Left  _ -> respond HTTP.unsupportedMediaType415
            Right t -> pure t

getSupportedContentType :: ApiAction s i Text
getSupportedContentType = do
    ct <- getContentType
    if ct `elem` contentTypes then
        pure ct
    else
        respond HTTP.unsupportedMediaType415


getBody :: (Serialise a, FromJSON a) => ApiAction s i a
getBody = do
    ct <- getSupportedContentType
    body <- getRawBody
    case decode ct body of
        Left _  -> respond HTTP.badRequest400
        Right a -> pure a
  where
    decode :: (Serialise a, FromJSON a) => Text -> LBS.ByteString -> Either Text a
    decode "application/json" bs = first T.pack          (Aeson.eitherDecode' bs)
    decode "application/cbor" bs = first (T.pack . show) (Serialise.deserialiseOrFail bs)
    decode _ _                   = Left $ "unknown content type"


notImplemented :: ApiAction s i ()
notImplemented =
    respondJson HTTP.notImplemented501 body
  where
    body = errorBody "Not implemented"

errorBody :: Text -> Aeson.Value
errorBody msg = Aeson.object ["error" .= msg]

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