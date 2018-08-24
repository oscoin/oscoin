module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashable, Hashed, hash)
import           Oscoin.HTTP.Internal
import           Oscoin.Data.Operation (Operation(..))
import qualified Oscoin.Node as Node
import           Oscoin.Node.Mempool.Class (lookupTx, addTxs)
import qualified Oscoin.Crypto.PubKey as PubKey

import           Codec.Serialise
import           Data.Aeson (FromJSON, ToJSON)
import           Network.HTTP.Types.Status

root :: ApiAction tx s i ()
root = respond ok200

getAllTransactions :: ToJSON tx => ApiAction tx s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respondJson ok200 mp

getTransaction :: (Hashable tx, ToJSON tx) => Hashed tx -> ApiAction tx s i ()
getTransaction txId = do
    mtx <- node (lookupTx txId)
    case mtx of
        Just tx -> respondJson ok200 tx
        Nothing -> respond notFound404

submitTransaction :: (Hashable tx, FromJSON tx, Serialise tx) => ApiAction tx s i ()
submitTransaction = do
    h <- getHeader "Accept"
    case h of
        Just "application/json" -> submitTransactionJson
        Just "application/cbor" -> submitTransactionCbor
        _                       -> respond notAcceptable406

submitTransactionJson :: (Hashable tx, FromJSON tx) => ApiAction tx s i ()
submitTransactionJson = do
    -- TODO: Create a pattern for this.
    result :: Maybe tx <- getBody
    case result of
        Just tx -> do
            -- TODO: Verify signature passed in url.
            receipt <- node $ do
                addTxs [tx]
                pure $ Node.Receipt (hash tx)
            respondJson accepted202 receipt
        Nothing ->
            respond badRequest400

submitTransactionCbor :: forall tx s i. (Hashable tx, Serialise tx) => ApiAction tx s i ()
submitTransactionCbor = do
    body <- getRawBody
    case deserialiseOrFail body of
        Left _ ->
            respond badRequest400
        Right Operation{opMessage} -> do
            receipt <- node $ do
                addTxs [PubKey.unsign opMessage]
                pure $ Node.Receipt (hash opMessage)
            respondCbor accepted202 receipt

-- | Runs a NodeT action in a MonadApi monad.
node :: MonadApi tx s i m => Node.NodeT tx s i IO a -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
