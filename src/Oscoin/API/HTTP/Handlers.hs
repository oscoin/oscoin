module Oscoin.API.HTTP.Handlers where

import           Oscoin.Prelude

import           Lens.Micro.Mtl (view)
import           Oscoin.API.HTTP.Internal
import           Oscoin.API.Types
import           Oscoin.Crypto.Blockchain (TxLookup(..))
import           Oscoin.Crypto.Blockchain.Block (BlockHash, Sealed, StateHash)
import           Oscoin.Crypto.Blockchain.Eval (Receipt(..))
import           Oscoin.Crypto.Hash (Hashed, hash)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Data.RadicleTx as RadicleTx
import           Oscoin.Data.Tx (verifyTx)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.State.Tree (Key)
import           Oscoin.Storage.Receipt.Class
import           Oscoin.Telemetry (telemetryStoreL)
import           Oscoin.Telemetry as Telemetry

import           Codec.Serialise (Serialise)
import           Data.Aeson (FromJSON, ToJSON)
import           Formatting.Buildable (Buildable)
import           Network.HTTP.Types.Status

root :: ApiAction c s i ()
root = respond ok200 noBody

getAllTransactions
    :: ( ToJSON (RadTx c)
       , Ord (Crypto.Hash c)
       , Serialise (BlockHash c)
       , Serialise (Crypto.PK c)
       , Serialise (Crypto.Signature c)
       )
     => ApiAction c s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respond ok200 $ body (Ok mp)

getTransaction :: ( Serialise (BlockHash c)
                  , Serialise (Crypto.PK c)
                  , Serialise (Crypto.Signature c)
                  , Crypto.HasHashing c
                  , Ord (Crypto.Hash c)
                  , ToJSON (Crypto.PK c)
                  , ToJSON (Crypto.Signature c)
                  , ToJSON (BlockHash c)
                  )
               => Hashed c (RadTx c)
               -> ApiAction c s i ()
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
                result <- Node.lookupTx id
                pure $ fromTxLookup <$> result

submitTransaction
    :: forall c s i a.
       ( Ord (Crypto.Hash c)
       , Serialise (BlockHash c)
       , Serialise (Crypto.PK c)
       , Serialise (Crypto.Signature c)
       , Crypto.HasHashing c
       , Buildable (Crypto.Hash c)
       , ToJSON (Crypto.Hash c)
       , FromJSON (BlockHash c)
       , FromJSON (Crypto.PK c)
       , FromJSON (Crypto.Signature c)
       , Crypto.HasDigitalSignature c
       )
    => ApiAction c s i a
submitTransaction = do
    tx <- getVerifiedTxBody
    receipt <- node $ do
        store <- view telemetryStoreL
        Mempool.addTxs [tx]
        forM_ [ TxsAddedToMempoolEvent [Crypto.fromHashed $ Crypto.hash @c tx]
              , TxSubmittedEvent (Crypto.hash @c tx)] $
            liftIO . Telemetry.emit store
        pure $ TxSubmitResponse (hash @c tx)

    respond accepted202 $ body (Ok receipt)
  where
    getVerifiedTxBody = do
        tx <- getBody
        if verifyTx tx
        then pure tx
        else respond badRequest400 $ errBody "Invalid transaction signature"

getBestChain
    :: ( Serialise s
       , ToJSON (Crypto.Hash c)
       , Crypto.HasHashing c
       , Serialise (BlockHash c)
       , Serialise (Crypto.PK c)
       , Serialise (Crypto.Signature c)
       , ToJSON (Sealed c s)
       , ToJSON (Crypto.PK c)
       , ToJSON (Crypto.Signature c)
       , Eq (Crypto.Hash c)
       )
    => ApiAction c s i a
getBestChain = do
    n    <- fromMaybe 3 <$> param "depth"
    blks <- node $ Node.getBlocks n
    respond ok200 $ body (Ok blks)

getBlock
    :: ( Serialise s
       , ToJSON (Crypto.Hash c)
       , Crypto.HasHashing c
       , Serialise (BlockHash c)
       , Serialise (Crypto.PK c)
       , Serialise (Crypto.Signature c)
       , ToJSON (Sealed c s)
       , ToJSON (Crypto.PK c)
       , ToJSON (Crypto.Signature c)
       , Eq (Crypto.Hash c)
       )
    => BlockHash c -> ApiAction c s i a
getBlock h = do
    result <- node $ Node.lookupBlock h
    case result of
        Just blk ->
            respond ok200 $ body (Ok blk)
        Nothing ->
            respond notFound404 noBody

getStatePath
    :: ( Ord (StateHash c)
       , Crypto.HasHashing c
       )
    => Key
    -> ApiAction c s i ()
getStatePath _chain = do
    path <- listParam "q"
    result' <- node $ Node.getPathLatest path
    case result' of
        Just (_sh, val) ->
            respondCbor ok200 (Ok val)
        Nothing ->
            respond notFound404 $ errBody "Value not found"

-- | Runs a NodeT action in a MonadApi monad.
node
    :: (MonadApi c s i m)
    => Node.NodeT c (RadTx c) (RadicleTx.Env c) s i IO a
    -> m a
node s = withHandle $ \h ->
    Node.runNodeT h s
