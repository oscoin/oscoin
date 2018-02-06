module Oscoin.HTTP.Internal where

import           Oscoin.Prelude
import           Oscoin.Org (Org)
import qualified Oscoin.Node.State as State
import           Web.Spock
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types.Status as HTTP

-- | The global server state.
data State = State
    { stOrgs :: [Org] }
    deriving (Show)

-- | Storage connection handle.
type Handle = State.Handle

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction = SpockAction Handle () State

-- | The type of our api.
type Api = SpockM Handle () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi m = (HasSpock m, SpockConn m ~ Handle)

-- | Create an empty state.
mkState :: State
mkState = State { stOrgs = [] }

getBody :: Aeson.FromJSON a => ApiAction (Maybe a)
getBody = jsonBody

getRawBody :: ApiAction LByteString
getRawBody = LBS.fromStrict <$> body

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = runQuery

respond :: HTTP.Status -> Maybe Aeson.Value -> ApiAction ()
respond status (Just body) =
    respondBytes status (Aeson.encode body)
respond status Nothing =
    respondBytes status ""

respondBytes :: HTTP.Status -> LByteString -> ApiAction ()
respondBytes status body = do
    setStatus status
    lazyBytes body

notImplemented :: ApiAction ()
notImplemented =
    respond HTTP.notImplemented501 (Just body)
  where
    body = errorBody "Not implemented"

errorBody :: Text -> Aeson.Value
errorBody msg = Aeson.object ["error" .= msg]
