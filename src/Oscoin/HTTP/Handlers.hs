module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashed, hash)
import           Oscoin.HTTP.Internal
import qualified Oscoin.Node as Node
import           Oscoin.Node.Mempool.Class (lookupTx, addTxs)
import           Oscoin.State.Tree (Key)

import           Data.Aeson.Extended ()
import           Network.HTTP.Types.Status

root :: ApiAction s i ()
root = respond ok200

getAllTransactions :: ApiAction s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respondJson ok200 mp

getTransaction :: Hashed ApiTx -> ApiAction s i ()
getTransaction txId = do
    mtx <- node (lookupTx txId)
    case mtx of
        Just tx -> respondJson ok200 tx
        Nothing -> respond notFound404

submitTransaction :: ApiAction s i ()
submitTransaction = do
    tx <- getBody

    receipt <- node $ do
        addTxs [tx]
        pure $ Node.Receipt (hash tx)

    respondBody receipt

getStatePath :: Key -> ApiAction s i ()
getStatePath k = do
    result <- node $ Node.getPath [k]
    case result of
        Just val ->
            respondCbor ok200 val
        Nothing ->
            respond notFound404

-- | Runs a NodeT action in a MonadApi monad.
node :: MonadApi s i m => Node.NodeT ApiTx s i IO a -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
