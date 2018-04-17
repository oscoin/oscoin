module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude hiding (notImplemented)
import qualified Oscoin.Node.State as State
import qualified Oscoin.Node.State.Mempool as Mempool
import           Oscoin.State.Tree (Key)
import           Oscoin.Crypto.Hash (Hashable, Hashed)
import           Oscoin.HTTP.Internal
import           Oscoin.Account (Account, AccId, MemberId)
import qualified Oscoin.Account.Transaction as Account
import           Oscoin.Account.Repository (RepoId)

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
            receipt <- storage $ Account.submitTransaction tx
            respond accepted202 (Just (toJSON receipt))
        Nothing ->
            respond badRequest400 Nothing

-- | Get a data key under an account.
getAccountDataKey :: AccId -> Key -> ApiAction tx ()
getAccountDataKey acc key = do
    result <- storage $ State.getPath (acc : "data" : [key])
    case result of
        Just val ->
            respondBytes ok200 val
        Nothing ->
            respond notFound404 (Just $ errorBody "Not found")

-- | Runs a StorageT action in a MonadApi monad.
storage :: MonadApi tx m => State.StorageT tx IO a -> m a
storage s = withHandle $ \h ->
    State.runStorageT h s

getAccounts :: ApiAction tx ()
getAccounts = do
    State{stAccounts} <- getState
    respond ok200 $ Just $ toJSON $ map snd stAccounts

getAccount :: AccId -> ApiAction tx ()
getAccount accId = do
    State{stAccounts} <- getState
    case lookup accId stAccounts of
        Just acc ->
            respond ok200 (Just $ toJSON acc)
        Nothing ->
            respond notFound404 Nothing

createAccount :: AccId -> ApiAction tx ()
createAccount accId = do
    Just acc :: Maybe Account <- getBody
    storage $ State.setPath ["accounts", accId] (encode acc)
    respond created201 Nothing

getRepos :: AccId -> ApiAction tx ()
getRepos _ = notImplemented

getRepo :: AccId -> RepoId -> ApiAction tx ()
getRepo _ _ = notImplemented

getMember :: AccId -> MemberId -> ApiAction tx ()
getMember _ _ = notImplemented
