module Oscoin.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashed, hash)
import           Oscoin.Data.Query
import           Oscoin.HTTP.Internal
import qualified Oscoin.HTTP.API.Result as Result
import qualified Oscoin.Node as Node
import           Oscoin.Node.Mempool.Class (lookupTx, addTxs)
import           Oscoin.State.Tree (Key, keyToPath)

import           Network.HTTP.Types.Status
import           Codec.Serialise (Serialise)

root :: ApiAction s i ()
root = respond ok200 nobody

getAllTransactions :: ApiAction s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    ct <- negotiateContentType
    respond ok200 $ body $ encode ct $ Result.ok mp

getTransaction :: Hashed ApiTx -> ApiAction s i ()
getTransaction txId = do
    mtx <- node (lookupTx txId)
    ct  <- negotiateContentType
    case mtx of
        Just tx -> respond ok200 $ body $ encode ct $ Result.ok tx
        Nothing -> respond notFound404 nobody

submitTransaction :: ApiAction s i a
submitTransaction = do
    tx <- getBody @ApiTx
    ct <- negotiateContentType

    receipt <- node $ do
        addTxs [tx]
        pure $ Node.Receipt (hash tx)

    respond accepted202 $ body $ encode ct $ Result.ok receipt

getStatePath
    :: (Serialise (QueryVal s), Query s)
    => Key -> ApiAction s i ()
getStatePath k = do
    result <- node $ Node.getPath (keyToPath k)
    case result of
        Just val ->
            respond ok200 $ body $ cbor $ Result.ok val
        Nothing ->
            respond notFound404 nobody

-- | Runs a NodeT action in a MonadApi monad.
node :: MonadApi s i m => Node.NodeT ApiTx s i IO a -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
