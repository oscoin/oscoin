module Oscoin.HTTP.Internal where

import           Oscoin.Prelude
import           Oscoin.Environment
import           Oscoin.Org (Org, OrgId)
import qualified Oscoin.Node.State as State
import           Oscoin.Crypto.Hash (Hashed)
import qualified Web.Spock as Spock
import           Web.HttpApiData (FromHttpApiData)
import           Web.Spock (SpockAction, SpockM, HasSpock, SpockConn, runSpock, spock)
import           Web.Spock.Config (defaultSpockCfg, PoolOrConn(..), ConnBuilder(..), PoolCfg(..))
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

-- | The global server state.
data State = State
    { stOrgs :: [(OrgId, Org)] }
    deriving (Show)

-- | Storage connection handle.
type Handle = State.Handle

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction tx = SpockAction (Handle (Hashed tx) tx) () State

-- | The type of our api.
type Api tx = SpockM (Handle (Hashed tx) tx) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi tx m = (HasSpock m, SpockConn m ~ Handle (Hashed tx) tx)

-- | Create an empty state.
mkState :: State
mkState = State { stOrgs = [] }

getBody :: Aeson.FromJSON a => ApiAction tx (Maybe a)
getBody = Spock.jsonBody

getRawBody :: ApiAction tx LByteString
getRawBody = LBS.fromStrict <$> Spock.body

getState :: ApiAction tx State
getState = Spock.getState

param' :: (FromHttpApiData p) => Text -> ApiAction tx p
param' = Spock.param'

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = Spock.runQuery

respond :: HTTP.Status -> Maybe Aeson.Value -> ApiAction tx ()
respond status (Just body) =
    respondBytes status (Aeson.encode body)
respond status Nothing =
    respondBytes status ""

respondBytes :: HTTP.Status -> LByteString -> ApiAction tx ()
respondBytes status body = do
    Spock.setStatus status
    Spock.lazyBytes body

notImplemented :: ApiAction tx ()
notImplemented =
    respond HTTP.notImplemented501 (Just body)
  where
    body = errorBody "Not implemented"

errorBody :: Text -> Aeson.Value
errorBody msg = Aeson.object ["error" .= msg]

run :: Ord tx => Api tx () -> [(OrgId, Org)] -> Int -> IO ()
run app orgs port =
    runSpock port (mkMiddleware app orgs)

mkMiddleware :: Ord tx => Api tx () -> [(OrgId, Org)] -> IO Wai.Middleware
mkMiddleware app orgs = do
    spockCfg <- defaultSpockCfg () (PCConn connBuilder) state
    spock spockCfg app
  where
    conn        = State.connect ()
    connBuilder = ConnBuilder conn State.close (PoolCfg 1 1 30)
    state       = mkState { stOrgs = orgs }

loggingMiddleware :: Environment -> Wai.Middleware
loggingMiddleware Production = Wai.logStdout
loggingMiddleware Development = Wai.logStdoutDev
loggingMiddleware Testing = id
