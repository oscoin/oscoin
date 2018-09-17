module Oscoin.API.HTTP.Internal where

import           Oscoin.Prelude

import           Oscoin.Environment
import qualified Oscoin.Node as Node
import           Oscoin.API.Types

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List (lookup)
import qualified Data.Text as T
import qualified Network.HTTP.Media as HTTP
import           Network.HTTP.Media (Quality, matchQuality, parseAccept, parseQuality, (//))
import qualified Network.HTTP.Types.Status as HTTP
import           Network.HTTP.Types.Header (HeaderName)
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import           Web.HttpApiData (FromHttpApiData)
import           Web.Spock (HasSpock, SpockAction, SpockConn, SpockM, runSpock, spock)
import qualified Web.Spock as Spock
import           Web.Spock.Config (ConnBuilder(..), PoolCfg(..), PoolOrConn(..), defaultSpockCfg)

-- | The global server state.
data State = State ()
    deriving (Show)

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction s i = SpockAction (Node.Handle RadTx s i) () State

instance MonadFail (ApiAction s i) where
    fail = error "failing!"

-- | The type of our api.
type Api s i = SpockM (Node.Handle RadTx s i) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi s i m = (HasSpock m, SpockConn m ~ Node.Handle RadTx s i)

-- | Create an empty state.
mkState :: State
mkState = State ()

getHeader :: HeaderName -> ApiAction s i (Maybe BS.ByteString)
getHeader = Spock.rawHeader

getHeader' :: HeaderName -> ApiAction s i BS.ByteString
getHeader' name = do
    header <- getHeader name
    case header of
        Just v  -> pure v
        Nothing -> respond HTTP.badRequest400 noBody

getRawBody :: ApiAction s i LBS.ByteString
getRawBody = LBS.fromStrict <$> Spock.body

-- | A sum type of supported media types.
data MediaType = JSON | CBOR deriving (Ord, Eq, Show)

fromMediaType :: MediaType -> HTTP.MediaType
fromMediaType JSON = "application" // "json"
fromMediaType CBOR = "application" // "cbor"

parseMediaType :: BS.ByteString -> Either Text MediaType
parseMediaType t = toEither $ do
    accepted <- parseAccept t
    lookup accepted $ NonEmpty.toList supportedMediaTypes
    where toEither = maybe (Left err) Right
          err = "Content-Type '" ++ T.pack (show t) ++ "' not supported."

supportedMediaTypes :: NonEmpty (HTTP.MediaType, MediaType)
supportedMediaTypes = NonEmpty.fromList $ [(fromMediaType ct, ct) | ct <- [JSON, CBOR]]

-- | Gets the parsed content types out of the Accept header, ordered by priority.
getAccepted :: ApiAction s i [Quality HTTP.MediaType]
getAccepted = do
    accept <- maybe "application/json" identity <$> getHeader "Accept"
    case parseQuality accept of
       Nothing -> respond HTTP.notAcceptable406 noBody
       Just accepted -> pure accepted

-- | Negotiates the best response content type from the request's accept header.
negotiateContentType :: ApiAction s i MediaType
negotiateContentType = do
    accepted <- getAccepted
    let supported = NonEmpty.toList supportedMediaTypes
    let offered = fst <$> supported
    case matchQuality offered accepted of
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

encode :: (Serialise a, ToJSON a) => MediaType -> a -> LBS.ByteString
encode JSON = Aeson.encode
encode CBOR = Serialise.serialise

decode :: (Serialise a, FromJSON a) => MediaType -> LBS.ByteString -> Either Text a
decode JSON bs = first T.pack          (Aeson.eitherDecode' bs)
decode CBOR bs = first (T.pack . show) (Serialise.deserialiseOrFail bs)

respond :: (ToJSON a, Serialise a) => HTTP.Status -> Maybe a -> ApiAction s i b
respond status (Just bdy) = do
    ct <- negotiateContentType
    respondBytes status ct (encode ct bdy)
respond status Nothing = do
    Spock.setStatus status
    Spock.lazyBytes ""

respondBytes :: HTTP.Status -> MediaType -> LBS.ByteString -> ApiAction s i b
respondBytes status ct bdy = do
    Spock.setStatus status
    Spock.setHeader "Content-Type" $ T.pack $ show $ fromMediaType ct
    Spock.lazyBytes bdy

getSupportedContentType :: ApiAction s i MediaType
getSupportedContentType = (parseMediaType <$> getHeader' "Content-Type") >>= \case
    Left _   -> respond HTTP.unsupportedMediaType415 noBody
    Right mt -> pure mt

getBody :: (Serialise a, FromJSON a) => ApiAction s i a
getBody = do
    ct <- getSupportedContentType
    !body' <- getRawBody
    case decode ct body' of
        Left  _ -> respond HTTP.badRequest400 $ body $ Err @() "Failed to decode body"
        Right a -> pure a

notImplemented :: ApiAction s i ()
notImplemented = respond HTTP.notImplemented501 noBody

run :: Api s i ()
    -> Int
    -> Node.Handle RadTx s i
    -> IO ()
run app port hdl =
    runSpock port (mkMiddleware app hdl)

mkMiddleware
    :: Api s i ()
    -> Node.Handle RadTx s i
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
