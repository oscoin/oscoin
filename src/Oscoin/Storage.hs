module Oscoin.Storage
    ( Storage(..)
    , hoistStorage

    , ApplyResult(..)

    , applyBlock
    , applyTx
    , lookupTx

    , isNovelTx
    , isNovelBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Storage.Block.Abstract (BlockStore, isNovelBlock)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.State.Class (MonadStateStore)
import           Oscoin.Storage.State.Class as StateStore

import           Oscoin.Consensus (Validate, validateBlockSize)
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain hiding (lookupTx)
import           Oscoin.Crypto.Blockchain.Eval (Evaluator, evalBlock)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Telemetry (NotableEvent(..))

import           Codec.Serialise (Serialise)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Foldable (toList)

-- | Package up the storage operations in a dictionary
data Storage tx s f = Storage
    { storageApplyBlock  :: Block tx s  -> f ApplyResult
    , storageApplyTx     :: tx          -> f ApplyResult
    , storageLookupBlock :: BlockHash   -> f (Maybe (Block tx s))
    , storageLookupTx    :: Hashed tx   -> f (Maybe tx)
    }

-- | Transform the 'Storage' monad
hoistStorage :: (forall a. m a -> n a) -> Storage tx s m -> Storage tx s n
hoistStorage f s = s
    { storageApplyBlock  = f . storageApplyBlock  s
    , storageApplyTx     = f . storageApplyTx     s
    , storageLookupBlock = f . storageLookupBlock s
    , storageLookupTx    = f . storageLookupTx    s
    }

data ApplyResult =
      Applied [NotableEvent]
    | Stale   [NotableEvent]
    | Error   [NotableEvent]

applyBlock
    :: ( MonadStateStore st   m
       , MonadMempool    tx   m
       , Crypto.Hashable tx
       , Serialise       tx
       , Serialise       s
       )
    => BlockStore tx s m
    -> Evaluator st tx o
    -> Validate     tx s
    -> Consensus.Config
    -> Block        tx s
    -> m ApplyResult
applyBlock bs eval validate config blk = do
    let blkHash = blockHash blk
    novel <- isNovelBlock bs blkHash
    if | novel -> do
        blks <- BlockStore.getBlocks bs 2016 -- XXX(alexis)
        case validateBlockSize config blk >>= const (validate blks blk) of
            Left err -> pure $ Error [BlockValidationFailedEvent blkHash err]
            Right () -> do
                let txs = blockData blk
                BlockStore.insertBlock bs blk
                Mempool.delTxs txs

                -- Try to find the parent state of the block, and if found,
                -- save a new state in the state store.
                result <- runMaybeT $ do
                    prevBlock <- MaybeT $ BlockStore.lookupBlock bs
                        (blockPrevHash $ blockHeader blk)

                    prevState <- MaybeT $ StateStore.lookupState
                        (blockStateHash $ blockHeader prevBlock)

                    case evalBlock eval prevState blk of
                        Left err ->
                            pure $ Error [BlockEvaluationFailedEvent blkHash err]
                        Right st' -> do
                            lift $ StateStore.storeState st'
                            let events = [ BlockAppliedEvent blkHash
                                         , TxsRemovedFromMempoolEvent (toList txs)
                                         ]
                            pure $ Applied events

                -- Nb. If either the parent block wasn't found, or the parent state,
                -- the block is considered an "orphan", and we consider it
                -- 'Applied' anyway.
                pure $ maybe (Applied [BlockOrphanEvent blkHash]) identity result

       | otherwise -> pure $ Stale [BlockStaleEvent blkHash]

applyTx
    :: ( MonadMempool    tx   m
       , Crypto.Hashable tx
       )
    => BlockStore tx s m
    -> tx
    -> m ApplyResult
applyTx bs tx = do
    let txHash = Crypto.hash tx
    novel <- isNovelTx bs txHash
    if | novel     -> Mempool.addTxs [tx] $> Applied [TxAppliedEvent txHash]
       | otherwise -> pure $ Stale [TxStaleEvent txHash]

lookupTx
    :: ( MonadMempool    tx   m )
    => BlockStore tx s m
    -> Hashed tx
    -> m (Maybe tx)
lookupTx bs hsh =
    runMaybeT $
            MaybeT (BlockStore.lookupTx bs hsh <&> map txPayload)
        <|> MaybeT (Mempool.lookupTx    hsh)

--------------------------------------------------------------------------------

isNovelTx :: MonadMempool tx m => BlockStore tx s m -> Hashed tx -> m Bool
isNovelTx bs = map isNothing . lookupTx bs
