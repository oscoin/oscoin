module Oscoin.HTTP.Internal where

import           Oscoin.Prelude
import qualified Oscoin.Node.State as State
import           Web.Spock
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types.Status as HTTP

-- | The global server state.
newtype State = State ()

-- | Storage connection handle.
type Handle = State.Handle

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction = SpockAction Handle () State

-- | The type of our api.
type Api = SpockM Handle () State

-- | Represents any monad which can act like an ApiAction.
type MonadApi m = (HasSpock m, SpockConn m ~ Handle)

getBody :: Aeson.FromJSON a => ApiAction (Maybe a)
getBody = jsonBody

getRawBody :: ApiAction LByteString
getRawBody = LBS.fromStrict <$> body

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = runQuery

respond :: Aeson.ToJSON a => HTTP.Status -> Maybe a -> ApiAction ()
respond status (Just body) =
    respondRaw status (Aeson.encode body)
respond status Nothing =
    respondRaw status ""

respondRaw :: HTTP.Status -> LByteString -> ApiAction ()
respondRaw status body = do
    setStatus status
    lazyBytes body

emptyBody :: Maybe ()
emptyBody = Nothing
