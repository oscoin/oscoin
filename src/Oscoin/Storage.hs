module Oscoin.Storage
    ( Storage(..)
    , hoistStorage

    , ApplyResult(..)

    , applyBlock
    , applyTx
    , lookupBlock
    , lookupTx

    , isNovelTx
    , isNovelBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Storage.Block.Class (MonadBlockStore)
import qualified Oscoin.Storage.Block.Class as BlockStore
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
    { storageApplyBlock  :: Block tx s  -> f ([NotableEvent], ApplyResult)
    , storageApplyTx     :: tx          -> f ([NotableEvent], ApplyResult)
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
      Applied
    | Stale
    | Error -- TODO(kim): figure out informative error types
    deriving Eq

applyBlock
    :: ( MonadBlockStore tx s m
       , MonadStateStore st   m
       , MonadMempool    tx   m
       , Crypto.Hashable tx
       , Serialise       tx
       , Serialise       s
       )
    => Evaluator st tx o
    -> Validate     tx s
    -> Consensus.Config
    -> Block        tx s
    -> m ([NotableEvent], ApplyResult)
applyBlock eval validate config blk = do
    let blkHash = blockHash blk
    novel <- isNovelBlock blkHash
    if | novel -> do
        blks <- BlockStore.getBlocks 2016 -- XXX(alexis)
        case validateBlockSize config blk >>= const (validate blks blk) of
            Left err -> pure ([BlockValidationFailedEvent blkHash err], Error)
            Right () -> do
                let txs = blockData blk
                BlockStore.storeBlock blk
                Mempool.delTxs txs

                -- Try to find the parent state of the block, and if found,
                -- save a new state in the state store.
                result <- runMaybeT $ do
                    prevBlock <- MaybeT $ BlockStore.lookupBlock
                        (blockPrevHash $ blockHeader blk)

                    prevState <- MaybeT $ StateStore.lookupState
                        (blockStateHash $ blockHeader prevBlock)

                    case evalBlock eval prevState blk of
                        Left err ->
                            pure ([BlockEvaluationFailedEvent blkHash err], Error)
                        Right st' -> do
                            lift $ StateStore.storeState st'
                            let events = [ BlockAppliedEvent blkHash
                                         , TxsRemovedFromMempoolEvent (toList txs)
                                         ]
                            pure (events, Applied)

                -- Nb. If either the parent block wasn't found, or the parent state,
                -- the block is considered an "orphan", and we consider it
                -- 'Applied' anyway.
                pure $ maybe ([BlockOrphanEvent blkHash], Applied) identity result

       | otherwise -> pure ([BlockStaleEvent blkHash], Stale)

applyTx
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       , Crypto.Hashable tx
       )
    => tx
    -> m ([NotableEvent], ApplyResult)
applyTx tx = do
    let txHash = Crypto.hash tx
    novel <- isNovelTx txHash
    if | novel     -> Mempool.addTxs [tx] $> ([TxAppliedEvent txHash], Applied)
       | otherwise -> pure ([TxStaleEvent txHash], Stale)

lookupBlock :: MonadBlockStore tx s m => BlockHash -> m (Maybe (Block tx s))
lookupBlock = BlockStore.lookupBlock

lookupTx
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       )
    => Hashed tx
    -> m (Maybe tx)
lookupTx hsh =
    runMaybeT $
            MaybeT (BlockStore.lookupTx hsh <&> map txPayload)
        <|> MaybeT (Mempool.lookupTx    hsh)

--------------------------------------------------------------------------------

isNovelBlock :: (MonadBlockStore tx s m) => BlockHash -> m Bool
isNovelBlock = map isNothing . lookupBlock

isNovelTx :: (MonadBlockStore tx s m, MonadMempool tx m) => Hashed tx -> m Bool
isNovelTx = map isNothing . lookupTx
