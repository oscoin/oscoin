module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude
import qualified Oscoin.Node.State as State
import           Oscoin.HTTP.Internal (MonadApi, ApiAction, getBody, withHandle)
import           Oscoin.Org (OrgId, OrgKey, OrgVal)

-- | Get a key under an organization.
getOrgKey :: OrgId -> OrgKey -> ApiAction ()
getOrgKey _org _key = notImplemented

-- | Set a key under an organization.
setOrgKey :: OrgId -> OrgKey -> ApiAction ()
setOrgKey org key = do
    val :: OrgVal <- getBody
    store $ State.setOrgKey org key val

-- | Runs a StorageT action in a MonadApi monad.
store :: MonadApi m => State.StorageT IO a -> m a
store s = withHandle $ \h ->
    State.runStorageT h s
