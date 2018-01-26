module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude
import qualified Oscoin.Node.State as State
import           Oscoin.HTTP.Internal (MonadApi, ApiAction, getBody, withHandle)
import           Oscoin.Org (OrgId, OrgKey, OrgVal, MemberId)
import           Oscoin.Org.Repository (RepoId)

-- | Get a key under an organization.
getOrgKey :: OrgId -> OrgKey -> ApiAction ()
getOrgKey _org _key = notImplemented

-- | Set a key under an organization.
setOrgKey :: OrgId -> OrgKey -> ApiAction ()
setOrgKey org key = do
    val :: OrgVal <- getBody
    store $ State.setOrgKey org key val

-- | Delete a key under an organization.
deleteOrgKey :: OrgId -> OrgKey -> ApiAction ()
deleteOrgKey = notImplemented

-- | Runs a StorageT action in a MonadApi monad.
store :: MonadApi m => State.StorageT IO a -> m a
store s = withHandle $ \h ->
    State.runStorageT h s

getOrgs :: ApiAction ()
getOrgs = notImplemented

getOrg :: OrgId -> ApiAction ()
getOrg = notImplemented

createOrg :: OrgId -> ApiAction ()
createOrg = notImplemented

updateOrg :: OrgId -> ApiAction ()
updateOrg = notImplemented

deleteOrg :: OrgId -> ApiAction ()
deleteOrg = notImplemented

getRepos :: OrgId -> ApiAction ()
getRepos = notImplemented

getRepo :: OrgId -> RepoId -> ApiAction ()
getRepo = notImplemented

createRepo :: OrgId -> RepoId -> ApiAction ()
createRepo = notImplemented

updateRepo :: OrgId -> RepoId -> ApiAction ()
updateRepo = notImplemented

deleteRepo :: OrgId -> RepoId -> ApiAction ()
deleteRepo = notImplemented

submitPatch :: OrgId -> RepoId -> ApiAction ()
submitPatch = notImplemented

getMember :: OrgId -> MemberId -> ApiAction ()
getMember = notImplemented

createMember :: OrgId -> MemberId -> ApiAction ()
createMember = notImplemented

updateMember :: OrgId -> MemberId -> ApiAction ()
updateMember = notImplemented

deleteMember :: OrgId -> MemberId -> ApiAction ()
deleteMember = notImplemented
