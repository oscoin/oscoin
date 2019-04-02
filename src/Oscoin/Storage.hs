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

import           Oscoin.Storage.Block.Abstract (BlockStoreReader, isNovelBlock)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.State.Class (MonadStateStore)

import           Oscoin.Consensus (ValidationError, validateBlockSize)
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain hiding (lookupTx)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool
import           Oscoin.Telemetry (NotableEvent(..))

import           Codec.Serialise (Serialise)
import           Control.Monad.Trans.Maybe (MaybeT(..))
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
    :: forall c st tx s m.
       ( Serialise tx
       , Serialise s
       , Serialise (Crypto.Hash c)
       , MonadStateStore c st   m
       , MonadMempool    c tx   m
       , Crypto.Hashable c tx
       , Crypto.Hashable c (BlockHeader c (Sealed c s))
       , Buildable (Crypto.Hash c)
       )
    => BlockStoreReader c tx s m
    -> (Block c tx (Sealed c s) -> m ())
    -- ^ Called when block is new and valid
    -> (Block c tx (Sealed c s) -> Either (ValidationError c) ())
    -> Consensus.Config
    -> Block      c tx (Sealed c s)
    -> m ApplyResult
applyBlock bs dispatchBlock validateBasic config blk = do
    let blkHash = blockHash blk
    novel <- isNovelBlock bs blkHash
    if | novel ->
        -- Performs a basic validation on the incoming block, and push it
        -- into the queue for the 'StorageManager' to process it.
        case validateBlockSize config blk >>= const (validateBasic blk) of
            Left err -> pure $ Error [BlockValidationFailedEvent blkHash err]
            Right () -> do
                let txs = blockData blk

                dispatchBlock blk
                Mempool.delTxs txs
                pure $ Applied [BlockAppliedEvent blkHash]

       | otherwise -> pure $ Stale [BlockStaleEvent blkHash]

applyTx
    :: ( MonadMempool    c tx   m
       , Crypto.Hashable c tx
       , Buildable (Crypto.Hash c)
       )
    => BlockStoreReader c tx s m
    -> tx
    -> m ApplyResult
applyTx bs tx = do
    let txHash = Crypto.hash tx
    novel <- isNovelTx bs txHash
    if | novel     -> Mempool.addTxs [tx] $> Applied [TxAppliedEvent txHash]
       | otherwise -> pure $ Stale [TxStaleEvent txHash]

lookupTx
    :: ( MonadMempool c tx m )
    => BlockStoreReader c tx s m
    -> Hashed c tx
    -> m (Maybe tx)
lookupTx bs hsh =
    runMaybeT $
            MaybeT (BlockStore.lookupTx bs hsh <&> map txPayload)
        <|> MaybeT (Mempool.lookupTx    hsh)

--------------------------------------------------------------------------------

isNovelTx :: MonadMempool c tx m => BlockStoreReader c tx s m -> Hashed c tx -> m Bool
isNovelTx bs = map isNothing . lookupTx bs
