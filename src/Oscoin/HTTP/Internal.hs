module Oscoin.HTTP.Internal where

import           Oscoin.Prelude
import           Oscoin.Org (Org, OrgId)
import qualified Oscoin.Node.State as State
import qualified Web.Spock as Spock
import           Web.Spock (SpockAction, SpockM, HasSpock, SpockConn)
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types.Status as HTTP

-- | The global server state.
data State = State
    { stOrgs :: [(OrgId, Org)] }
    deriving (Show)

-- | Storage connection handle.
type Handle = State.Handle

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction tx = SpockAction (Handle tx) () State

-- | The type of our api.
type Api tx = SpockM (Handle tx) () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi tx m = (HasSpock m, SpockConn m ~ Handle tx)

-- | Create an empty state.
mkState :: State
mkState = State { stOrgs = [] }

getBody :: Aeson.FromJSON a => ApiAction tx (Maybe a)
getBody = Spock.jsonBody

getRawBody :: ApiAction tx LByteString
getRawBody = LBS.fromStrict <$> Spock.body

getState :: ApiAction tx State
getState = Spock.getState

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
