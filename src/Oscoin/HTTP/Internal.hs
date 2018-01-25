module Oscoin.HTTP.Internal where

import           Oscoin.Prelude
import           Web.Spock

-- | The global server state.
newtype State = State ()

-- | Storage connection.
type Connection = ()

-- | The type of all actions (effects) in our HTTP handlers.
type ApiAction = SpockAction Connection () State

-- | The type of our api.
type Api = SpockM Connection () State

getBody :: ApiAction ByteString
getBody = body

