module Oscoin.API.HTTP.Handlers where

import           Oscoin.Prelude

import           Oscoin.API.HTTP.Internal
import           Oscoin.API.Types
import           Oscoin.Crypto.Blockchain (Height)
import           Oscoin.Crypto.Blockchain.Block (BlockHash, SealedBlock)
import           Oscoin.Crypto.Hash (hash)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Telemetry (telemetryStoreL)
import           Oscoin.Telemetry as Telemetry
import qualified Oscoin.Time.Chrono as Chrono

import           Codec.Serialise (Serialise)
import           Data.ByteString.BaseN (decodeBase16)
import           Formatting.Buildable (Buildable)
import           Lens.Micro.Mtl (view)
import           Network.HTTP.Types.Status

root :: ApiAction c tx s i ()
root = respond ok200 noBody

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

lookupHashesByHeight
    :: Serialise (BlockHash c)
    => ApiAction c tx s i a
lookupHashesByHeight = do
    start  <- param' "start"
    end    <- param' "end"
    hashes <- liftNode $ Node.lookupHashesByHeight (start, end)
    respond ok200 $ Ok (Chrono.toOldestFirst hashes)

lookupBlockByHeight
    :: (Serialise (SealedBlock c tx s))
    => Height
    -> ApiAction c tx s i a
lookupBlockByHeight h = do
    result <- liftNode $ Node.lookupBlockByHeight h
    case result of
        Just blk ->
            respond ok200 $ Ok blk
        Nothing ->
            respond notFound404 $ errBody "Block not found"

lookupBlocksByHeight
    :: (Serialise (SealedBlock c tx s))
    => ApiAction c tx s i a
lookupBlocksByHeight = do
    start <- param' "start"
    end   <- param' "end"
    blocks <- liftNode $ Node.lookupBlocksByHeight (start, end)
    respond ok200 $ Ok (Chrono.toOldestFirst blocks)

getStateValue
    :: (ApiTx c tx)
    => Text
    -> ApiAction c tx s i ()
getStateValue encodedKey =
    case decodeBase16 (toS encodedKey) of
        Nothing -> respond badRequest400 $ errBody "Invalid hex string"
        Just key -> do
            result <- liftNode $ Node.getStateValue (toS key)
            respond (statusCode result) result
  where
    statusCode (Just _) = ok200
    statusCode Nothing  = notFound404
