module Oscoin.HTTP.Internal where

import           Oscoin.Prelude

import           Oscoin.Environment
import qualified Oscoin.Node as Node

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
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
type ApiAction tx s i = SpockAction (Node.Handle tx s i) () State

instance MonadFail (ApiAction tx s i) where
    fail = error "failing!"

-- | The type of our api.
type Api tx s i = SpockM (Node.Handle tx s i) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi tx s i m = (HasSpock m, SpockConn m ~ Node.Handle tx s i)

-- | Create an empty state.
mkState :: State
mkState = State ()

getBody :: Aeson.FromJSON a => ApiAction tx s i (Maybe a)
getBody = Spock.jsonBody

getRawBody :: ApiAction tx s i LByteString
getRawBody = LBS.fromStrict <$> Spock.body

getState :: ApiAction tx s i State
getState = Spock.getState

param' :: (FromHttpApiData p) => Text -> ApiAction tx s i p
param' = Spock.param'

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = Spock.runQuery

respond :: HTTP.Status -> Maybe Aeson.Value -> ApiAction tx s i ()
respond status (Just body) =
    respondBytes status (Aeson.encode body)
respond status Nothing =
    respondBytes status ""

respondBytes :: HTTP.Status -> LByteString -> ApiAction tx s i ()
respondBytes status body = do
    Spock.setStatus status
    Spock.lazyBytes body

notImplemented :: ApiAction tx s i ()
notImplemented =
    respond HTTP.notImplemented501 (Just body)
  where
    body = errorBody "Not implemented"

errorBody :: Text -> Aeson.Value
errorBody msg = Aeson.object ["error" .= msg]

run :: Api tx s i ()
    -> Int
    -> Node.Handle tx s i
    -> IO ()
run app port hdl =
    runSpock port (mkMiddleware app hdl)

mkMiddleware
    :: Api tx s i ()
    -> Node.Handle tx s i
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
