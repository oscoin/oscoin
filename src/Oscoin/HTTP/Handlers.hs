module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude hiding (notImplemented)
import qualified Oscoin.Node.State as State
import           Oscoin.HTTP.Internal
import           Oscoin.Org (Org, OrgId, OrgKey, OrgVal, MemberId)
import           Oscoin.Org.Repository (RepoId)

import           Data.Aeson (encode)
import           Data.Aeson.Types (emptyArray)
import           Network.HTTP.Types.Status

-- | Get a key under an organization.
getOrgKey :: OrgId -> OrgKey -> ApiAction ()
getOrgKey org key = do
    result <- storage $ State.getOrgKey org key
    case result of
        Just val ->
            respondRaw ok200 val
        Nothing ->
            respond notFound404 (Just $ errorBody "Not found")

-- | Set a key under an organization.
setOrgKey :: OrgId -> OrgKey -> ApiAction ()
setOrgKey org key = do
    val :: OrgVal <- getRawBody
    storage $ State.setOrgKey org key val

-- | Delete a key under an organization.
deleteOrgKey :: OrgId -> OrgKey -> ApiAction ()
deleteOrgKey _ _ = notImplemented

-- | Runs a StorageT action in a MonadApi monad.
storage :: MonadApi m => State.StorageT IO a -> m a
storage s = withHandle $ \h ->
    State.runStorageT h s

getOrgs :: ApiAction ()
getOrgs =
    respond ok200 (Just emptyArray)

getOrg :: OrgId -> ApiAction ()
getOrg orgId = do
    result <- storage $ State.getKey ["orgs", orgId]
    case result of
        Just org ->
            respondRaw ok200 org
        Nothing ->
            respond notFound404 emptyBody

createOrg :: OrgId -> ApiAction ()
createOrg orgId = do
    Just org :: Maybe Org <- getBody
    storage $ State.setKey ["orgs", orgId] (encode org)
    respond created201 emptyBody

updateOrg :: OrgId -> ApiAction ()
updateOrg _ = notImplemented

deleteOrg :: OrgId -> ApiAction ()
deleteOrg _ = notImplemented

getRepos :: OrgId -> ApiAction ()
getRepos _ = notImplemented

getRepo :: OrgId -> RepoId -> ApiAction ()
getRepo _ _ = notImplemented

createRepo :: OrgId -> RepoId -> ApiAction ()
createRepo _ _ = notImplemented

updateRepo :: OrgId -> RepoId -> ApiAction ()
updateRepo _ _ = notImplemented

deleteRepo :: OrgId -> RepoId -> ApiAction ()
deleteRepo _ _ = notImplemented

submitPatch :: OrgId -> RepoId -> ApiAction ()
submitPatch _ _ = notImplemented

getMember :: OrgId -> MemberId -> ApiAction ()
getMember _ _ = notImplemented

createMember :: OrgId -> MemberId -> ApiAction ()
createMember _ _ = notImplemented

updateMember :: OrgId -> MemberId -> ApiAction ()
updateMember _ _ = notImplemented

deleteMember :: OrgId -> MemberId -> ApiAction ()
deleteMember _ _ = notImplemented
