module Oscoin.HTTP.Internal where

import           Oscoin.Prelude
import qualified Oscoin.Node.State as State
import           Web.Spock
import qualified Data.Aeson as Aeson
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

getBody :: ApiAction ByteString
getBody = body

-- | Runs an action by passing it a handle.
withHandle :: HasSpock m => (SpockConn m -> IO a) -> m a
withHandle = runQuery

respond :: Aeson.ToJSON a => HTTP.Status -> a -> ApiAction ()
respond status body = do
    setStatus status
    json body
