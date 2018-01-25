module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude
import qualified Oscoin.Node.State as State
import           Oscoin.HTTP.Internal (MonadApi, ApiAction, getBody, withConn)
import           Oscoin.Org (OrgId, OrgKey, OrgVal)

getOrgKey :: OrgId -> OrgKey -> ApiAction ()
getOrgKey _org _key = notImplemented

setOrgKey :: OrgId -> OrgKey -> ApiAction ()
setOrgKey org key = do
    val :: OrgVal <- getBody
    store $ State.setOrgKey org key val

store :: MonadApi m => State.StorageT IO a -> m a
store s = withConn $ \conn ->
    State.runStorageT conn s
