module Oscoin.API.HTTP.Handlers where

import           Oscoin.Prelude

import           Lens.Micro.Mtl (view)
import           Oscoin.API.HTTP.Internal
import           Oscoin.API.Types
import           Oscoin.Crypto.Blockchain (TxLookup(..))
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Blockchain.Eval (Receipt(..))
import           Oscoin.Crypto.Hash (Hashed, hash)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (Tx)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Telemetry (telemetryStoreL)
import           Oscoin.Telemetry as Telemetry

import           Codec.Serialise (Serialise)
import           Formatting.Buildable (Buildable)
import           Network.HTTP.Types.Status

root :: ApiAction c s i ()
root = respond ok200 noBody

getAllTransactions
    :: ( Ord (Crypto.Hash c)
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       )
     => ApiAction c s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respond ok200 $ Ok mp

getTransaction :: ( Serialise (BlockHash c)
                  , Serialise (Crypto.PublicKey c)
                  , Serialise (Crypto.Signature c)
                  , Crypto.HasHashing c
                  )
               => Hashed c (Tx c)
               -> ApiAction c s i ()
getTransaction txId = do
    (tx, bh, confirmations) <- node (lookupTx txId) >>= \case
        Nothing -> respond notFound404 $ errBody "Transaction not found"
        Just res -> pure $ res
    txReceipt <- node $ Node.lookupReceipt txId
    respond ok200 $ Ok TxLookupResponse
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
                result <- Node.lookupTx id
                pure $ fromTxLookup <$> result

submitTransaction
    :: forall c s i a.
       ( Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , Crypto.HasHashing c
       , Buildable (Crypto.Hash c)
       )
    => ApiAction c s i a
submitTransaction = do
    tx <- getBody
    let txHash = hash @c tx
    addResult <- node $ Mempool.addTx tx
    telemetryStore <- node $ view telemetryStoreL
    case addResult of
        Left _ -> do
            liftIO $ Telemetry.emit telemetryStore $ TxSubmittedInvalidEvent txHash
            respond badRequest400 $ errBody "Invalid transaction"
        Right _ -> do
            forM_ [ TxsAddedToMempoolEvent [Crypto.fromHashed txHash]
                  , TxSubmittedEvent txHash] $
                liftIO . Telemetry.emit telemetryStore
            respond accepted202 $ Ok $ TxSubmitResponse txHash

getBestChain
    :: ( Serialise s
       , Crypto.HasHashing c
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       )
    => ApiAction c s i a
getBestChain = do
    n    <- fromMaybe 3 <$> param "depth"
    blks <- node $ Node.getBlocks n
    respond ok200 $ Ok blks

getTip
    :: ( Serialise s
       , Crypto.HasHashing c
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       )
    => ApiAction c s i a
getTip = do
    tip <- node $ Node.getTip
    respond ok200 $ Ok tip

getBlock
    :: ( Serialise s
       , Crypto.HasHashing c
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       )
    => BlockHash c -> ApiAction c s i a
getBlock h = do
    result <- node $ Node.lookupBlock h
    case result of
        Just blk ->
            respond ok200 $ Ok blk
        Nothing ->
            respond notFound404 $ errBody "Block not found"

getStatePath :: (Crypto.HasHashing c, Serialise (BlockHash c), Serialise (Crypto.PublicKey c), Serialise (Crypto.Signature c)) => Text -> ApiAction c s i ()
getStatePath _chain = do
    path <- listParam "q"
    result' <- node $ Node.getPathLatest path
    case result' of
        Just val ->
            respond ok200 (Ok val)
        Nothing ->
            respond notFound404 $ errBody "Value not found"

-- | Runs a NodeT action in a MonadApi monad.
node
    :: (MonadApi c s i m)
    => Node.NodeT c (Tx c) s i IO a
    -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
