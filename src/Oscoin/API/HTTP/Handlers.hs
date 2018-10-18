module Oscoin.API.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.API.HTTP.Internal
import           Oscoin.API.Types
import           Oscoin.Crypto.Blockchain (TxLookup(..))
import qualified Oscoin.Crypto.Blockchain as Blockchain
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Blockchain.Eval (Receipt(..))
import           Oscoin.Crypto.Hash (Hashed, hash)
import qualified Oscoin.Data.RadicleTx as RadicleTx
import           Oscoin.Data.Tx (verifyTx)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.State.Tree (Key)
import qualified Oscoin.Storage.Block.Class as BlockStore
import           Oscoin.Storage.Receipt.Class

import           Codec.Serialise (serialise)
import           Network.HTTP.Types.Status

root :: ApiAction i ()
root = respond ok200 noBody

getAllTransactions :: ApiAction i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respond ok200 $ body (Ok mp)

getTransaction :: Hashed RadTx -> ApiAction i ()
getTransaction txId = do
    (tx, bh, confirmations) <- node (lookupTx txId) >>= \case
        Nothing -> respond notFound404 $ errBody "Transaction not found"
        Just res -> pure $ res
    txReceipt <- node $ lookupReceipt txId
    respond ok200 $ body $ Ok TxLookupResponse
        { txHash = hash tx
        , txBlockHash =  bh
        , txOutput = receiptTxOutput <$> txReceipt
        , txConfirmations = confirmations
        , txPayload = tx
        }
    where
        fromTxLookup TxLookup{..} = (txPayload, Just txBlockHash, txConfirmations)
        lookupTx id = Mempool.lookupTx id >>= \case
            Just tx -> pure $ Just (tx, Nothing, 0)
            Nothing -> do
                chain <- BlockStore.maximumChainBy (comparing Blockchain.height)
                pure $ fromTxLookup <$> Blockchain.lookupTx id chain


submitTransaction :: ApiAction i a
submitTransaction = do
    tx <- getVerifiedTxBody
    receipt <- node $ do
        Mempool.addTxs [tx]
        pure $ TxSubmitResponse (hash tx)

    respond accepted202 $ body (Ok receipt)
  where
    getVerifiedTxBody = do
        tx <- getBody
        if verifyTx tx
        then pure tx
        else respond badRequest400 $ errBody "Invalid transaction signature"

getBestChain :: ApiAction i a
getBestChain = do
    n    <- fromMaybe 3 <$> param "depth"
    blks <- node $ take n . Blockchain.blocks <$> Node.getBestChain
    respond ok200 (body $ Ok $ map void blks)

getBlock :: BlockHash -> ApiAction i a
getBlock h = do
    result <- node $ BlockStore.lookupBlock h
    case result of
        Just blk ->
            respond ok200 (body . Ok $ void blk)
        Nothing ->
            respond notFound404 noBody

getStatePath :: Key -> ApiAction i ()
getStatePath _chain = do
    result <- decodeListParam <$> param' "q"
    case result of
        Left err ->
            respond badRequest400 (errBody err)
        Right path -> do
            result' <- node $ Node.getPath path
            case result' of
                Just val ->
                    respondBytes ok200 CBOR (serialise $ Ok val)
                Nothing ->
                    respond notFound404 $ errBody "Value not found"

-- | Runs a NodeT action in a MonadApi monad.
node :: MonadApi i m => Node.NodeT RadTx RadicleTx.Env i IO a -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
