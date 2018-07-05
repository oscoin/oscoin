module Oscoin.HTTP.Handlers where

import           Oscoin.Account (AccId, MemberId)
import           Oscoin.Account.Repository (RepoId)
import qualified Oscoin.Account.Transaction as Account
import           Oscoin.Crypto.Hash (Hashable, Hashed)
import           Oscoin.HTTP.Internal
import qualified Oscoin.Node as Node
import           Oscoin.Node.Mempool.Class (lookupTx)
import           Oscoin.Prelude hiding (notImplemented)
import           Oscoin.State.Tree (Key)

import           Data.Aeson (FromJSON, ToJSON, toJSON)
import           Network.HTTP.Types.Status

getAllTransactions :: ToJSON tx => ApiAction tx i ()
getAllTransactions = do
    mp <- storage Node.getMempool
    respond ok200 (Just (toJSON mp))

getTransaction :: (Hashable tx, ToJSON tx) => Hashed tx -> ApiAction tx i ()
getTransaction txId = do
    mtx <- storage (lookupTx txId)
    case mtx of
        Just tx -> respond ok200 (Just (toJSON tx))
        Nothing -> respond notFound404 Nothing

submitTransaction :: (Hashable tx, FromJSON tx, Id tx ~ Hashed tx) => ApiAction tx i ()
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
getAccountDataKey :: AccId -> Key -> ApiAction tx i ()
getAccountDataKey acc key = do
    result <- storage $ Node.getPath (acc : "data" : [key])
    case result of
        Just val ->
            respondBytes ok200 val
        Nothing ->
            respond notFound404 (Just $ errorBody "Not found")

-- | Runs a StorageT action in a MonadApi monad.
storage :: MonadApi tx i m => Node.NodeT tx i IO a -> m a
storage s = withHandle $ \h ->
    Node.runNodeT h s

getAccounts :: ApiAction tx i ()
getAccounts = do
    State{stAccounts} <- getState
    respond ok200 $ Just $ toJSON $ map snd stAccounts

getAccount :: AccId -> ApiAction tx i ()
getAccount accId = do
    State{stAccounts} <- getState
    case lookup accId stAccounts of
        Just acc ->
            respond ok200 (Just $ toJSON acc)
        Nothing ->
            respond notFound404 Nothing

getRepos :: AccId -> ApiAction tx i ()
getRepos _ = notImplemented

getRepo :: AccId -> RepoId -> ApiAction tx i ()
getRepo _ _ = notImplemented

getMember :: AccId -> MemberId -> ApiAction tx i ()
getMember _ _ = notImplemented
