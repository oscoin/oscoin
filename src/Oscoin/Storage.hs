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
import           Formatting.Buildable (Buildable)

-- | Package up the storage operations in a dictionary
data Storage c tx s f = Storage
    { storageApplyBlock  :: Block c tx (Sealed c s) -> f ApplyResult
    , storageApplyTx     :: tx                      -> f ApplyResult
    , storageLookupBlock :: BlockHash c             -> f (Maybe (Block c tx (Sealed c s)))
    , storageLookupTx    :: Hashed c tx             -> f (Maybe tx)
    }

-- | Transform the 'Storage' monad
hoistStorage :: (forall a. m a -> n a) -> Storage c tx s m -> Storage c tx s n
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
    :: forall c st tx s o m.
       ( Eq (Crypto.Hash c)
       , Serialise tx
       , Serialise s
       , Serialise (Crypto.Hash c)
       , MonadStateStore c st   m
       , MonadMempool    c tx   m
       , Crypto.Hashable c tx
       , Crypto.Hashable c (BlockHeader c (Sealed c s))
       , Buildable (Crypto.Hash c)
       )
    => BlockStore c tx s m
    -> Evaluator    st tx o
    -> Validate   c tx s
    -> Consensus.Config
    -> Block      c tx (Sealed c s)
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
                            let hashes = map (Crypto.fromHashed . Crypto.hash @c) $ toList txs
                            let events = [ BlockAppliedEvent blkHash
                                         , TxsRemovedFromMempoolEvent hashes
                                         ]
                            pure $ Applied events

                -- Nb. If either the parent block wasn't found, or the parent state,
                -- the block is considered an "orphan", and we consider it
                -- 'Applied' anyway.
                pure $ maybe (Applied [BlockOrphanEvent blkHash]) identity result

       | otherwise -> pure $ Stale [BlockStaleEvent blkHash]

applyTx
    :: ( MonadMempool    c tx   m
       , Crypto.Hashable c tx
       , Buildable (Crypto.Hash c)
       )
    => BlockStore c tx s m
    -> tx
    -> m ApplyResult
applyTx bs tx = do
    let txHash = Crypto.hash tx
    novel <- isNovelTx bs txHash
    if | novel     -> Mempool.addTxs [tx] $> Applied [TxAppliedEvent txHash]
       | otherwise -> pure $ Stale [TxStaleEvent txHash]

lookupTx
    :: ( MonadMempool c tx m )
    => BlockStore c tx s m
    -> Hashed c tx
    -> m (Maybe tx)
lookupTx bs hsh =
    runMaybeT $
            MaybeT (BlockStore.lookupTx bs hsh <&> map txPayload)
        <|> MaybeT (Mempool.lookupTx    hsh)

--------------------------------------------------------------------------------

isNovelTx :: MonadMempool c tx m => BlockStore c tx s m -> Hashed c tx -> m Bool
isNovelTx bs = map isNothing . lookupTx bs
