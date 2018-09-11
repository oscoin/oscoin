module Oscoin.API.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashed, hash)
import           Oscoin.Data.Query
import           Oscoin.API.HTTP.Internal
import           Oscoin.API.HTTP.Response (GetTxResponse(..))
import           Oscoin.API.Types
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.State.Tree (Key, keyToPath)

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import           Network.HTTP.Types.Status
import           Codec.Serialise (Serialise, serialise)

root :: ApiAction s i ()
root = respond ok200 noBody

getAllTransactions :: ApiAction s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respond ok200 $ body (Ok mp)

getTransaction :: Hashed RadTx -> ApiAction s i ()
getTransaction txId = node (lookupTx txId) >>= \case
    Nothing -> respond notFound404 noBody
    Just tx -> respond ok200 $ body $ Ok GetTxResponse
        { txHash = hash tx
        , txBlockHash = Nothing
        , txConfirmations = confirmations tx
        , txPayload = tx
        }
    where
        confirmations _ = 0 -- FIXME(tsenart)
        lookupTx id = runMaybeT
             $  MaybeT (Mempool.lookupTx id)
            <|> MaybeT (BlockStore.lookupTx id)

submitTransaction :: ApiAction s i a
submitTransaction = do
    tx <- getBody @RadTx

    receipt <- node $ do
        Mempool.addTxs [tx]
        pure $ Node.Receipt (hash tx)

    respond accepted202 $ body (Ok receipt)

getStatePath
    :: (Serialise (QueryVal s), Query s)
    => Key -> ApiAction s i ()
getStatePath k = do
    result <- node $ Node.getPath (keyToPath k)
    case result of
        Just val ->
            respondBytes ok200 CBOR (serialise $ Ok val)
        Nothing ->
            respond notFound404 noBody

-- | Runs a NodeT action in a MonadApi monad.
node :: MonadApi s i m => Node.NodeT RadTx s i IO a -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
