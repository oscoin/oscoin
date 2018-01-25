module Oscoin.HTTP.Internal where

import           Oscoin.Prelude
import qualified Oscoin.Node.State as State
import           Web.Spock

-- | The global server state.
newtype State = State ()

-- | Storage connection.
type Connection = State.Handle

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction = SpockAction Connection () State

-- | The type of our api.
type Api = SpockM Connection () State

getBody :: ApiAction ByteString
getBody = body

type MonadApi m = (HasSpock m, SpockConn m ~ Connection)

withConn :: HasSpock m => (SpockConn m -> IO a) -> m a
withConn = runQuery
