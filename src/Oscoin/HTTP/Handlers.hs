module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashable, Hashed, hash)
import           Oscoin.HTTP.Internal
import qualified Oscoin.Node as Node
import           Oscoin.Node.Mempool.Class (lookupTx, addTxs)

import           Data.Aeson (FromJSON, ToJSON, toJSON)
import           Network.HTTP.Types.Status

getAllTransactions :: ToJSON tx => ApiAction tx s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respond ok200 (Just (toJSON mp))

getTransaction :: (Hashable tx, ToJSON tx) => Hashed tx -> ApiAction tx s i ()
getTransaction txId = do
    mtx <- node (lookupTx txId)
    case mtx of
        Just tx -> respond ok200 (Just (toJSON tx))
        Nothing -> respond notFound404 Nothing

submitTransaction :: (Hashable tx, FromJSON tx) => ApiAction tx s i ()
submitTransaction = do
    -- TODO: Create a pattern for this.
    result :: Maybe tx <- getBody
    case result of
        Just tx -> do
            -- TODO: Verify signature passed in url.
            receipt <- node $ do
                addTxs [tx]
                pure $ Node.Receipt (hash tx)
            respond accepted202 (Just (toJSON receipt))
        Nothing ->
            respond badRequest400 Nothing

-- | Runs a NodeT action in a MonadApi monad.
node :: MonadApi tx s i m => Node.NodeT tx s i IO a -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
