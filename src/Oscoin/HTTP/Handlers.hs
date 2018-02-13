module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude hiding (notImplemented)
import qualified Oscoin.Node.State as State
import           Oscoin.Crypto.Hash (Hashable, Hashed)
import           Oscoin.HTTP.Internal
import qualified Oscoin.Transaction.Mempool as Mempool
import           Oscoin.Org (Org, OrgId, OrgKey, MemberId, mkOrgDataPath)
import qualified Oscoin.Org.Transaction as Org
import           Oscoin.Org.Repository (RepoId)

import           Data.Aeson (FromJSON, ToJSON, encode, toJSON)
import           Network.HTTP.Types.Status

getAllTransactions :: (ToJSON (Id tx), ToJSON tx) => ApiAction tx ()
getAllTransactions = do
    mp <- storage $ State.getMempool
    respond ok200 (Just (toJSON mp))

getTransaction :: (Ord (Id tx), ToJSON tx) => Id tx -> ApiAction tx ()
getTransaction txId = do
    mp <- storage $ State.getMempool
    case Mempool.lookup txId mp of
        Just tx -> respond ok200 (Just (toJSON tx))
        Nothing -> respond notFound404 Nothing

submitTransaction :: (Hashable tx, FromJSON tx, Id tx ~ Hashed tx) => ApiAction tx ()
submitTransaction = do
    -- TODO: Create a pattern for this.
    result :: Maybe tx <- getBody
    case result of
        Just tx -> do
            -- TODO: Verify signature passed in url.
            receipt <- storage $ Org.submitTransaction tx
            respond accepted202 (Just (toJSON receipt))
        Nothing ->
            respond badRequest400 Nothing

-- | Get a key under an organization.
getOrgKey :: OrgId -> OrgKey -> ApiAction tx ()
getOrgKey org key = do
    result <- storage $ State.getPath (mkOrgDataPath org [key])
    case result of
        Just val ->
            respondBytes ok200 val
        Nothing ->
            respond notFound404 (Just $ errorBody "Not found")

-- | Runs a StorageT action in a MonadApi monad.
storage :: MonadApi tx m => State.StorageT tx IO a -> m a
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

getRepos :: OrgId -> ApiAction tx ()
getRepos _ = notImplemented

getRepo :: OrgId -> RepoId -> ApiAction tx ()
getRepo _ _ = notImplemented

getMember :: OrgId -> MemberId -> ApiAction tx ()
getMember _ _ = notImplemented
