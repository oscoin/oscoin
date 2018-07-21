module Oscoin.HTTP.Internal where

import           Oscoin.Prelude

import           Oscoin.Account (AccId, Account)
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
data State = State
    { stAccounts :: [(AccId, Account)] }
    deriving (Show)

-- | Storage connection handle.
type Handle tx = Node.Handle tx Node.Root

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction tx i = SpockAction (Handle tx i) () State

instance MonadFail (ApiAction tx i) where
    fail = error "failing!"

-- | The type of our api.
type Api tx i = SpockM (Handle tx i) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi tx i m = (HasSpock m, SpockConn m ~ Handle tx i)

-- | Create an empty state.
mkState :: State
mkState = State { stAccounts = [] }

getBody :: Aeson.FromJSON a => ApiAction tx i (Maybe a)
getBody = Spock.jsonBody

getRawBody :: ApiAction tx i LByteString
getRawBody = LBS.fromStrict <$> Spock.body

getState :: ApiAction tx i State
getState = Spock.getState

param' :: (FromHttpApiData p) => Text -> ApiAction tx i p
param' = Spock.param'

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = Spock.runQuery

respond :: HTTP.Status -> Maybe Aeson.Value -> ApiAction tx i ()
respond status (Just body) =
    respondBytes status (Aeson.encode body)
respond status Nothing =
    respondBytes status ""

respondBytes :: HTTP.Status -> LByteString -> ApiAction tx i ()
respondBytes status body = do
    Spock.setStatus status
    Spock.lazyBytes body

notImplemented :: ApiAction tx i ()
notImplemented =
    respond HTTP.notImplemented501 (Just body)
  where
    body = errorBody "Not implemented"

errorBody :: Text -> Aeson.Value
errorBody msg = Aeson.object ["error" .= msg]

run :: Api tx i ()
    -> [(AccId, Account)]
    -> Int
    -> Handle tx i
    -> IO ()
run app accs port hdl =
    runSpock port (mkMiddleware app accs hdl)

mkMiddleware
    :: Api tx i ()
    -> [(AccId, Account)]
    -> Handle tx i
    -> IO Wai.Middleware
mkMiddleware app accs hdl = do
    spockCfg <- defaultSpockCfg () (PCConn connBuilder) state
    spock spockCfg app
  where
    connBuilder = ConnBuilder (pure hdl) (const pass) (PoolCfg 1 1 30)
    state       = mkState { stAccounts = accs }

loggingMiddleware :: Environment -> Wai.Middleware
loggingMiddleware Production = Wai.logStdout
loggingMiddleware Development = Wai.logStdoutDev
loggingMiddleware Testing = identity
