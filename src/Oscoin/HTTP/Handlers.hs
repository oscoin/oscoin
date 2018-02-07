module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude hiding (notImplemented)
import qualified Oscoin.Node.State as State
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.HTTP.Internal
import           Oscoin.Org (Org, OrgId, OrgKey, OrgVal, MemberId, mkOrgDataPath)
import qualified Oscoin.Org.Transaction as Org
import           Oscoin.Org.Repository (RepoId)

import           Data.Aeson (ToJSON, encode, toJSON)
import           Network.HTTP.Types.Status

getMempool :: ToJSON tx => ApiAction tx ()
getMempool = do
    mp <- storage $ State.getMempool
    respond ok200 (Just (toJSON mp))

-- | Get a key under an organization.
getOrgKey :: OrgId -> OrgKey -> ApiAction tx ()
getOrgKey org key = do
    result <- storage $ State.getPath (mkOrgDataPath org [key])
    case result of
        Just val ->
            respondBytes ok200 val
        Nothing ->
            respond notFound404 (Just $ errorBody "Not found")

-- | Set a key under an organization.
setOrgKey :: OrgId -> OrgKey -> ApiAction (Crypto.Signed Org.Tx) ()
setOrgKey org key = do
    val :: OrgVal <- getRawBody
    sig <- param' "sig"
    tx <- pure $ Crypto.signed sig (Org.setTx org key val)
    receipt <- storage $ Org.submitTransaction tx
    respond accepted202 (Just $ toJSON receipt)

-- | Delete a key under an organization.
deleteOrgKey :: OrgId -> OrgKey -> ApiAction tx ()
deleteOrgKey _ _ = notImplemented

-- | Runs a StorageT action in a MonadApi monad.
storage :: MonadApi tx m => State.StorageT (tx) IO a -> m a
storage s = withHandle $ \h ->
    State.runStorageT h s

getOrgs :: ApiAction tx ()
getOrgs = do
    State{stOrgs} <- getState
    respond ok200 $ Just $ toJSON $ map snd stOrgs

getOrg :: OrgId -> ApiAction tx ()
getOrg orgId = do
    State{stOrgs} <- getState
    case lookup orgId stOrgs of
        Just org ->
            respond ok200 (Just $ toJSON org)
        Nothing ->
            respond notFound404 Nothing

createOrg :: OrgId -> ApiAction tx ()
createOrg orgId = do
    Just org :: Maybe Org <- getBody
    storage $ State.setPath ["orgs", orgId] (encode org)
    respond created201 Nothing

updateOrg :: OrgId -> ApiAction tx ()
updateOrg _ = notImplemented

deleteOrg :: OrgId -> ApiAction tx ()
deleteOrg _ = notImplemented

getRepos :: OrgId -> ApiAction tx ()
getRepos _ = notImplemented

getRepo :: OrgId -> RepoId -> ApiAction tx ()
getRepo _ _ = notImplemented

createRepo :: OrgId -> RepoId -> ApiAction tx ()
createRepo _ _ = notImplemented

updateRepo :: OrgId -> RepoId -> ApiAction tx ()
updateRepo _ _ = notImplemented

deleteRepo :: OrgId -> RepoId -> ApiAction tx ()
deleteRepo _ _ = notImplemented

submitPatch :: OrgId -> RepoId -> ApiAction tx ()
submitPatch _ _ = notImplemented

getMember :: OrgId -> MemberId -> ApiAction tx ()
getMember _ _ = notImplemented

createMember :: OrgId -> MemberId -> ApiAction tx ()
createMember _ _ = notImplemented

updateMember :: OrgId -> MemberId -> ApiAction tx ()
updateMember _ _ = notImplemented

deleteMember :: OrgId -> MemberId -> ApiAction tx ()
deleteMember _ _ = notImplemented
