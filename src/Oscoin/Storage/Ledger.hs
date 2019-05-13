{-# LANGUAGE UndecidableInstances #-}

-- | A 'Ledger' provides access the states and receipts associated with
-- blocks in a 'BlockStoreReader'.
--
-- This module should be imported qualified.
module Oscoin.Storage.Ledger
    ( Ledger
    , hoist
    , mkLedger
    , newFromBlockStoreIO

    , blockStoreReader
    , getBlocksByDepth
    , getTipWithState
    , getTip
    , lookupState
    , lookupReceipt

    , buildNextBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Block.Abstract (BlockStoreReader)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.HashStore
import qualified Oscoin.Storage.Receipt as ReceiptStore
import qualified Oscoin.Time.Chrono as Chrono

import           Codec.Serialise (Serialise)
import           Control.Monad.Trans.Maybe
import qualified Crypto.Data.Auth.Tree.Class as AuthTree

-- | Errors that may be raised from the ledger's methods
data LedgerError c
    = StateNotInCache (BlockHash c)
    -- ^ State for the given block is not materialized in the state
    -- cache.
    | BlockNotFound (BlockHash c)
    -- ^ A block with the given hash was not found in the block store

deriving instance (Crypto.HasHashing c) => Eq (LedgerError c)
deriving instance (Show (Crypto.Hash c)) => Show (LedgerError c)

data Ledger crypto seal tx output state (m :: * -> *) = Ledger
    { ledgerBlockStoreReader :: BlockStoreReader crypto tx seal m
    , ledgerStateStore       :: HashStore crypto state m
    , ledgerReceiptStore     :: ReceiptStore.ReceiptStore crypto tx output m
    , ledgerEvaluator        :: Evaluator state tx output
    }

hoist
    :: (forall a. m a -> n a)
    -> Ledger crypto seal tx output state m
    -> Ledger crypto seal tx output state n
hoist f l =
    Ledger
        { ledgerBlockStoreReader = BlockStore.hoistBlockStoreReader f $ ledgerBlockStoreReader l
        , ledgerStateStore = hoistHashStore f $ ledgerStateStore l
        , ledgerReceiptStore = ReceiptStore.hoistReceiptStore f $ ledgerReceiptStore l
        , ledgerEvaluator = ledgerEvaluator l
        }


-- | Create a ledger backed by the given block store and in-memory
-- storage of state and receipts.
--
-- @initialState@ is the state that the transactions of the
-- genesisblock are applied to.
newFromBlockStoreIO
    :: ( MonadIO m
       , Crypto.Hashable crypto state
       , Crypto.Hashable crypto tx
       )
    => Evaluator state tx output
    -> BlockStoreReader crypto tx seal IO
    -> state
    -> IO (Ledger crypto seal tx output state m)
newFromBlockStoreIO ledgerEvaluator ledgerBlockStoreReader initialState = do
    ledgerReceiptStore <- ReceiptStore.newReceiptStoreIO
    ledgerStateStore <- newHashStoreIO
    storeHashContent ledgerStateStore initialState

    let ledger = Ledger{ledgerBlockStoreReader, ledgerStateStore, ledgerReceiptStore, ledgerEvaluator}
    materialize ledger initialState
    pure $ hoist liftIO ledger


mkLedger
    :: BlockStoreReader crypto tx seal m
    -> HashStore crypto state m
    -- ^ State store
    -> Evaluator state tx output
    -> ReceiptStore.ReceiptStore crypto tx output m
    -> Ledger crypto seal tx output state m
mkLedger ledgerBlockStoreReader ledgerStateStore ledgerEvaluator ledgerReceiptStore =
    Ledger
        { ledgerBlockStoreReader
        , ledgerEvaluator
        , ledgerStateStore
        , ledgerReceiptStore
        }


-----------------------------------------------------------
-- * Getters
-----------------------------------------------------------

blockStoreReader :: Ledger crypto seal tx output state m -> BlockStore.BlockStoreReader crypto tx seal m
blockStoreReader = ledgerBlockStoreReader

-- | Get the @depth@ most recent blocks.
getBlocksByDepth
    :: Ledger crypto seal tx output state m
    -> Depth
    -> m (Chrono.NewestFirst [] (Block crypto tx (Sealed crypto seal)))
getBlocksByDepth ledger depth = BlockStore.getBlocksByDepth (blockStoreReader ledger) depth

getTip :: Ledger crypto seal tx output state m -> m (Block crypto tx (Sealed crypto seal))
getTip ledger = BlockStore.getTip (ledgerBlockStoreReader ledger)


-- | Get the receipt for a given transaction contained in the
-- blockchain.
--
-- Returns 'Nothing' if the transaction is not contained in the
-- blockchain.
--
-- Also returns 'Nothing' if the receipt is in the blockchain but not
-- in the receipt cache. In ther future we will generate the receipt
-- if it is not in the cache.
lookupReceipt
    :: (Monad m, Crypto.Hashable crypto tx)
    => Ledger crypto seal tx output state m
    -> Crypto.Hashed crypto tx
    -> m (Maybe (ReceiptStore.Receipt crypto tx output))
lookupReceipt ledger txHash = runMaybeT $ do
    -- Ensure that the transaction is in the block chain. Just calling
    -- 'ReceiptStore.lookupReceipt' is not sufficient since
    -- 'buildNextBlock' adds receipts without the block being part of
    -- the blockchain.
    txLookup <- MaybeT $ BlockStore.lookupTx (blockStoreReader ledger) txHash
    block <- MaybeT $ BlockStore.lookupBlock (blockStoreReader ledger) (txBlockHash txLookup)
    _ <- lift $ materializeBlock ledger block
    MaybeT $ ReceiptStore.lookupReceipt (ledgerReceiptStore ledger) txHash


-- | Get the most recent block and the associated state.
getTipWithState
    :: (Monad m, Crypto.Hashable crypto tx, HasCallStack)
    => Ledger crypto seal tx output state m
    -> m (Either (LedgerError crypto) (Block crypto tx (Sealed crypto seal), state))
getTipWithState ledger = runExceptT $ do
    block <- lift $ getTip ledger
    st <- ExceptT $ materializeBlock ledger block
    pure (block, st)


lookupState
    :: Ledger crypto seal tx output state m
    -> Crypto.Hashed crypto state
    -> m (Maybe state)
lookupState Ledger{ledgerStateStore} stateHash = lookupHashContent ledgerStateStore stateHash


-----------------------------------------------------------
-- * Materialization
-----------------------------------------------------------

-- | Builds an unsealed block from a list of transactions applied to
-- the state of the tip.
--
-- This uses 'Oscoin.Crypto.Blockchain.Eval' internally which means
-- that invalid transactions are disregarded.
--
-- Used by 'Oscoin.Consensus.Mining.mineBlock'.
buildNextBlock
    :: ( Monad m
       , Serialise tx
       , Serialise (Beneficiary crypto)
       , Crypto.Hashable crypto tx
       , Crypto.Hashable crypto state
       , Crypto.Hashable crypto (BlockHeader crypto Unsealed)
       , AuthTree.MerkleHash (Crypto.Hash crypto)
       )
    => Ledger crypto seal tx output state m
    -> Timestamp
    -> Beneficiary crypto
    -> [tx]
    -> m (Either (LedgerError crypto) (Block crypto tx Unsealed))
buildNextBlock ledger time benef txs = runExceptT $ do
    (currentTip, tipState) <- ExceptT $ getTipWithState ledger
    let (newBlock, newState, receipts) =
            buildBlock (ledgerEvaluator ledger) time benef tipState txs currentTip
    lift $ do
        storeHashContent (ledgerStateStore ledger) newState
        forM_ receipts $ ReceiptStore.storeReceipt (ledgerReceiptStore ledger)
    pure newBlock


-- | Computes the state and receipts associated with the given block
-- and adds them to the storage.
--
-- The given state must be the state associated with the parent block
-- of the given block.
materializeBlockFromState
    :: (Monad m, Crypto.Hashable crypto tx)
    => Ledger crypto seal tx output state m
    -> state
    -> Block crypto tx (Sealed crypto seal)
    -> m state
materializeBlockFromState ledger prevState blk = do
    let (newState, receipts) = evalBlock (ledgerEvaluator ledger) prevState blk
    storeHashContent (ledgerStateStore ledger) newState
    forM_ receipts (ReceiptStore.storeReceipt (ledgerReceiptStore ledger))
    pure newState


-- | Computes the state and receipts associated with the given block
-- and adds them to the storage.
--
-- This is similar to 'materializeBlockFromState' but it looks up the
-- parent state in the cache and materializes it recursivley if not
-- present.
materializeBlock
    :: (Monad m, Crypto.Hashable crypto tx, HasCallStack)
    => Ledger crypto seal tx output state m
    -> Block crypto tx (Sealed crypto seal)
    -> m (Either (LedgerError crypto) state)
materializeBlock ledger blk = runExceptT $ do
    maybeBlockState <- lift $ lookupHashContent (ledgerStateStore ledger) (Crypto.toHashed $ blockStateHash $ blockHeader blk)
    case maybeBlockState of
        Just blockState -> pure blockState
        Nothing          -> do
            maybeParentBlock <- lift $ BlockStore.lookupBlock (ledgerBlockStoreReader ledger) (parentHash blk)
            parentBlock <- case maybeParentBlock of
                Just parentBlock -> pure $ parentBlock
                Nothing          -> throwError $ BlockNotFound $ parentHash blk
            parentState <- ExceptT $ materializeBlock ledger parentBlock
            lift $ materializeBlockFromState ledger parentState blk


-- | Computes all states and receipts from the blocks in the
-- blockchain and adds them to the storage.
--
-- The genesis block is applied to the given initial state
materialize
    :: (Monad m, Crypto.Hashable crypto tx)
    => Ledger crypto seal tx output state m
    -> state
    -> m ()
materialize ledger initialState = do
    genesisBlock <- BlockStore.getGenesisBlock (blockStoreReader ledger)
    allFollowingBlocks <- BlockStore.getBlocksByParentHash (blockStoreReader ledger) (blockHash genesisBlock)
    _ <- foldlM (materializeBlockFromState ledger) initialState (genesisBlock : Chrono.toOldestFirst (Chrono.reverse allFollowingBlocks))
    pure ()
