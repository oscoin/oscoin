module Oscoin.API.HTTP.Internal
    ( ApiAction
    , Api
    , MonadApi
    , runApi
    , withHandle

    , MediaType(..)
    , decode
    , encode
    , parseMediaType
    , fromMediaType

    , trailingSlashPolicy
    , indexPolicy
    , mkMiddleware
    , loggingMiddleware

    , errBody
    , respond
    , respondBytes
    , noBody
    , body

    , param
    , param'
    , listParam
    , getBody
    ) where

import           Oscoin.Prelude hiding (State, state)

import           Oscoin.API.Types
import qualified Oscoin.Data.RadicleTx as RadicleTx
import           Oscoin.Environment
import qualified Oscoin.Node as Node

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List (init, isSuffixOf, lookup)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           Network.HTTP.Media
                 (Quality, mapQuality, parseAccept, parseQuality, (//))
import qualified Network.HTTP.Media as HTTP
import           Network.HTTP.Types.Header (HeaderName)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Network.Wai.Middleware.Static as Wai
import           Web.HttpApiData (FromHttpApiData, parseQueryParam)
import           Web.Spock
                 (HasSpock, SpockAction, SpockConn, SpockM, runSpock, spock)
import qualified Web.Spock as Spock
import           Web.Spock.Config
                 ( ConnBuilder(..)
                 , PoolCfg(..)
                 , PoolOrConn(..)
                 , defaultSpockCfg
                 )

-- | The global server state.
data State = State ()
    deriving (Show)

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction i = SpockAction (Node.Handle RadTx RadicleTx.Env i) () State

-- | The type of our api.
type Api i = SpockM (Node.Handle RadTx RadicleTx.Env i) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi i m = (HasSpock m, SpockConn m ~ Node.Handle RadTx RadicleTx.Env i)

-- | Create an empty state.
mkState :: State
mkState = State ()

getHeader :: HeaderName -> ApiAction i (Maybe BS.ByteString)
getHeader = Spock.rawHeader

getHeader' :: HeaderName -> ApiAction i BS.ByteString
getHeader' name = do
    header <- getHeader name
    case header of
        Just v  -> pure v
        Nothing -> respond HTTP.badRequest400 noBody

getRawBody :: ApiAction i LBS.ByteString
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
          err = "Content-Type '" <> show t <> "' not supported."

supportedMediaTypes :: NonEmpty (HTTP.MediaType, MediaType)
supportedMediaTypes = NonEmpty.fromList $ [(fromMediaType ct, ct) | ct <- [JSON, CBOR]]

-- | Gets the parsed content types out of the Accept header, ordered by priority.
getAccepted :: ApiAction i [Quality HTTP.MediaType]
getAccepted = do
    accept <- maybe "*/*" identity <$> getHeader "Accept"
    case parseQuality accept of
       Nothing -> respond HTTP.badRequest400 $ Just ("Accept header malformed" :: Text)
       Just accepted -> pure accepted

-- | Negotiates the best response content type from the request's accept header.
negotiateContentType :: ApiAction i MediaType
negotiateContentType = do
    accepted <- getAccepted
    let supported = NonEmpty.toList supportedMediaTypes
    case mapQuality supported accepted of
        Nothing -> respond HTTP.notAcceptable406 noBody
        Just ct -> pure ct

param' :: (FromHttpApiData p) => Text -> ApiAction i p
param' = Spock.param'

param :: (FromHttpApiData p) => Text -> ApiAction i (Maybe p)
param = Spock.param

listParam :: (FromHttpApiData p) => Text -> ApiAction i [p]
listParam key = do
    p <- param' key
    case decodeList p of
        Left err    -> respond HTTP.badRequest400 (errBody err)
        Right value -> pure value
  where
    decodeList = traverse parseQueryParam . split . inner
    inner = T.dropWhile (== '[') . T.dropWhileEnd (== ']')
    split = T.splitOn ","

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = Spock.runQuery

body :: a -> Maybe a
body = Just

noBody :: Maybe ()
noBody = Nothing

errBody :: Text -> Maybe (Result ())
errBody = Just . Err

encode :: (Serialise a, ToJSON a) => MediaType -> a -> LBS.ByteString
encode JSON = Aeson.encode
encode CBOR = Serialise.serialise

decode :: (Serialise a, FromJSON a) => MediaType -> LBS.ByteString -> Either Text a
decode JSON bs = first T.pack          (Aeson.eitherDecode' bs)
decode CBOR bs = first (T.pack . show) (Serialise.deserialiseOrFail bs)

respond :: (ToJSON a, Serialise a) => HTTP.Status -> Maybe a -> ApiAction i b
respond status (Just bdy) = do
    ct <- negotiateContentType
    respondBytes status ct (encode ct bdy)
respond status Nothing = do
    Spock.setStatus status
    Spock.lazyBytes ""

respondBytes :: HTTP.Status -> MediaType -> LBS.ByteString -> ApiAction i b
respondBytes status ct bdy = do
    Spock.setStatus status
    Spock.setHeader "Content-Type" $ T.pack $ show $ fromMediaType ct
    Spock.lazyBytes bdy

getSupportedContentType :: ApiAction i MediaType
getSupportedContentType = (parseMediaType <$> getHeader' "Content-Type") >>= \case
    Left _   -> respond HTTP.unsupportedMediaType415 noBody
    Right mt -> pure mt

getBody :: (Serialise a, FromJSON a) => ApiAction i a
getBody = do
    ct <- getSupportedContentType
    !body' <- getRawBody
    case decode ct body' of
        Left  _ -> respond HTTP.badRequest400 $ body $ Err @() "Failed to decode body"
        Right a -> pure a

runApi :: Api i ()
    -> Int
    -> Node.Handle RadTx RadicleTx.Env i
    -> IO ()
runApi app port hdl =
    runSpock port (mkMiddleware app hdl)

mkMiddleware
    :: Api i ()
    -> Node.Handle RadTx RadicleTx.Env i
    -> IO Wai.Middleware
mkMiddleware app hdl = do
    spockCfg <- defaultSpockCfg () (PCConn connBuilder) state
    spock spockCfg app
  where
    connBuilder = ConnBuilder (pure hdl) (const pass) (PoolCfg 1 1 30)
    state       = mkState

loggingMiddleware :: Environment -> Wai.Middleware
loggingMiddleware Production  = Wai.logStdout
loggingMiddleware Development = Wai.logStdoutDev
loggingMiddleware Testing     = identity

-- | Static Policy to serve index.html files from directories.
indexPolicy :: String -> Wai.Policy
indexPolicy path =
    Wai.policy $ pure . change path (path ++ "/index.html")
  where
    change from to = (== from) >>= bool identity (const to)

-- | Static Policy to remove trailing slashes.
trailingSlashPolicy :: Wai.Policy
trailingSlashPolicy =
    Wai.policy f
  where
    f s | "/" `isSuffixOf` s = Just (init s)
        | otherwise          = Just s
