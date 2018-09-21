module Oscoin.API.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.API.HTTP.Internal
import           Oscoin.API.Types
import qualified Oscoin.Consensus.BlockStore.Class as BlockStore
import           Oscoin.Crypto.Blockchain (TxLookup(..))
import qualified Oscoin.Crypto.Blockchain as Blockchain
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import           Oscoin.Crypto.Hash (Hashed, hash)
import           Oscoin.Data.Tx (verifyTx)
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.State.Tree (Key, keyToPath)

import           Codec.Serialise (Serialise, serialise)
import           Network.HTTP.Types.Status

root :: ApiAction s i ()
root = respond ok200 noBody

getAllTransactions :: ApiAction s i ()
getAllTransactions = do
    mp <- node Node.getMempool
    respond ok200 $ body (Ok mp)

getTransaction :: Hashed RadTx -> ApiAction s i ()
getTransaction txId = node (lookupTx txId) >>= \case
    Nothing -> respond notFound404 noBody
    Just (tx, bh, confirmations) -> respond ok200 $ body $ Ok TxLookupResponse
        { txHash = hash tx
        , txBlockHash =  bh
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


submitTransaction :: ApiAction s i a
submitTransaction = do
    tx <- getVerifiedTxBody
    receipt <- node $ do
        Mempool.addTxs [tx]
        pure $ Node.Receipt (hash tx)

    respond accepted202 $ body (Ok receipt)
  where
    getVerifiedTxBody = do
        tx <- getBody
        if verifyTx tx
        then pure tx
        else respond badRequest400 $ body $ Err @() "Invalid transaction signature"

getBestChain :: ApiAction s i a
getBestChain = do
    n    <- fromMaybe 3 <$> param "depth"
    blks <- node $ take n . Blockchain.blocks <$> Node.getBestChain
    respond ok200 (body $ Ok $ map void blks)

getBlock :: BlockHash -> ApiAction s i a
getBlock h = do
    result <- node $ BlockStore.lookupBlock h
    case result of
        Just blk ->
            respond ok200 (body . Ok $ void blk)
        Nothing ->
            respond notFound404 noBody

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
