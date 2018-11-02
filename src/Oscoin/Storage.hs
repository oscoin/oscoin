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

import           Oscoin.Crypto.Blockchain hiding (lookupTx)
import           Oscoin.Crypto.Blockchain.Eval (Evaluator, evalBlock)
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool)
import qualified Oscoin.Node.Mempool.Class as Mempool

import           Codec.Serialise (Serialise)
import           Control.Monad.Trans.Maybe (MaybeT(..))

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
      Applied
    | Stale
    | Error -- TODO(kim): figure out informative error types
    deriving Eq

applyBlock
    :: ( MonadBlockStore tx s m
       , MonadStateStore st   m
       , MonadMempool    tx   m
       , Serialise          s
       )
    => Evaluator st tx o
    -> Block        tx s
    -> m ApplyResult
applyBlock eval blk = do
    novel <- isNovelBlock (blockHash blk)
    if | novel ->
        case validateBlock blk of
            Left  _    -> pure Error
            Right blk' -> do
                BlockStore.storeBlock blk'
                Mempool.delTxs (blockData blk')

                -- Try to find the parent state of the block, and if found,
                -- save a new state in the state store.
                result <- runMaybeT $ do
                    prevBlock <- MaybeT $ BlockStore.lookupBlock
                        (blockPrevHash $ blockHeader blk')

                    prevState <- MaybeT $ StateStore.lookupState
                        (blockStateHash $ blockHeader prevBlock)

                    case evalBlock eval prevState blk' of
                        Left _err ->
                            pure Error
                        Right st' -> do
                            lift $ StateStore.storeState st'
                            pure Applied

                -- Nb. If either the parent block wasn't found, or the parent state,
                -- the block is considered an "orphan", and we consider it
                -- 'Applied' anyway.
                pure $ maybe Applied identity result

       | otherwise -> pure Stale

applyTx
    :: ( MonadBlockStore tx s m
       , MonadMempool    tx   m
       , Crypto.Hashable tx
       )
    => tx
    -> m ApplyResult
applyTx tx = do
    novel <- isNovelTx (Crypto.hash tx)
    if | novel     -> Mempool.addTxs [tx] $> Applied
       | otherwise -> pure Stale

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
            MaybeT (BlockStore.lookupTx hsh)
        <|> MaybeT (Mempool.lookupTx    hsh)

--------------------------------------------------------------------------------

isNovelBlock :: (MonadBlockStore tx s m) => BlockHash -> m Bool
isNovelBlock = map isNothing . lookupBlock

isNovelTx :: (MonadBlockStore tx s m, MonadMempool tx m) => Hashed tx -> m Bool
isNovelTx = map isNothing . lookupTx
