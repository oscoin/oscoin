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
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
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

getAccept :: ApiAction s i Text
getAccept = do
    result <- getHeader "Accept"
    case result of
        Nothing     -> respond HTTP.notAcceptable406
        Just accept -> pure accept

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
    accept <- getAccept
    Spock.setHeader "Content-Type" accept
    case encode accept a of
        Nothing -> respond HTTP.notAcceptable406
        Just bs -> respondBytes HTTP.accepted202 bs
  where
    encode :: (Serialise a, ToJSON a) => Text -> a -> Maybe LBS.ByteString
    encode "application/json" = Just . Aeson.encode
    encode "application/cbor" = Just . Serialise.serialise
    encode _                  = const Nothing

getBody :: (Serialise a, FromJSON a) => ApiAction s i a
getBody = do
    ct   <- getHeader' "Content-Type"
    body <- getRawBody
    case decode ct body of
        Left _  -> respond HTTP.badRequest400
        Right a -> pure a
  where
    decode :: (Serialise a, FromJSON a) => Text -> LBS.ByteString -> Either Text a
    decode "application/json" bs = first T.pack          (Aeson.eitherDecode' bs)
    decode "application/cbor" bs = first (T.pack . show) (Serialise.deserialiseOrFail bs)
    decode contentType        _  = Left $ "unknown content type " ++ contentType

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
