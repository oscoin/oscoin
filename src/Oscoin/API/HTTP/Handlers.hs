module Oscoin.API.HTTP.Handlers where

import           Oscoin.Prelude

import           Lens.Micro.Mtl (view)
import           Oscoin.API.HTTP.Internal
import           Oscoin.API.Types
import           Oscoin.Crypto.Blockchain (TxLookup(..))
import           Oscoin.Crypto.Blockchain.Block (BlockHash, SealedBlock)
import           Oscoin.Crypto.Blockchain.Eval (Receipt(..))
import           Oscoin.Crypto.Hash (Hashed, hash)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Telemetry (telemetryStoreL)
import           Oscoin.Telemetry as Telemetry

import           Codec.Serialise (Serialise)
import           Formatting.Buildable (Buildable)
import           Network.HTTP.Types.Status

root :: ApiAction c tx s i ()
root = respond ok200 noBody

getTransaction
    :: ( ApiTx c tx
       , Serialise (TxLookupResponse c tx)
       )
    => Hashed c tx
    -> ApiAction c tx s i ()
getTransaction txId = do
    (tx, bh, confirmations) <- liftNode (lookupTx txId) >>= \case
        Nothing -> respond notFound404 $ errBody "Transaction not found"
        Just res -> pure $ res
    txReceipt <- liftNode $ Node.lookupReceipt txId
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
    :: forall c tx s i a.
       ( Serialise (BlockHash c)
       , Crypto.HasHashing c
       , Buildable (Crypto.Hash c)
       , ApiTx c tx
       )
    => ApiAction c tx s i a
submitTransaction = do
    tx <- getBody
    let txHash = hash @c tx
    addResult <- liftNode $ Mempool.addTx tx
    telemetryStore <- liftNode $ view telemetryStoreL
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
    :: (Serialise (SealedBlock c tx s))
    => ApiAction c tx s i a
getBestChain = do
    n    <- fromMaybe 3 <$> param "depth"
    blks <- liftNode $ Node.getBlocks n
    respond ok200 $ Ok blks

getTip
    :: (Serialise (SealedBlock c tx s))
    => ApiAction c tx s i a
getTip = do
    tip <- liftNode $ Node.getTip
    respond ok200 $ Ok tip

getBlock
    :: (Serialise (SealedBlock c tx s))
    => BlockHash c -> ApiAction c tx s i a
getBlock h = do
    result <- liftNode $ Node.lookupBlock h
    case result of
        Just blk ->
            respond ok200 $ Ok blk
        Nothing ->
            respond notFound404 $ errBody "Block not found"

getStatePath
    :: (ApiTx c tx)
    => Text
    -> ApiAction c tx s i ()
getStatePath _chain = do
    path <- listParam "q"
    result' <- liftNode $ Node.getPathLatest path
    case result' of
        Just val ->
            respond ok200 (Ok val)
        Nothing ->
            respond notFound404 $ errBody "Value not found"
