-- | This module provides the 'ReceiptStore' interface which is a
-- 'ContentStore' that uses the transaction hashes to identify
-- transaction receipts.
--
-- For legacy reasons we also provide the 'MonadReceiptStore' type
-- class to obtain a receipt store.
module Oscoin.Storage.Receipt
    ( MonadReceiptStore(..)
    , addReceipt
    , lookupReceipt

    , ReceiptStore
    , ReceiptMap
    , newReceiptStoreIO
    , mkStateReceiptStore

    -- * Re-exports from "Oscoin.Storage.ContentStore"
    , storeContent
    , lookupContent
    , hoistContentStore
    ) where

import           Oscoin.Prelude

import           Lens.Micro
import           Oscoin.Crypto.Blockchain.Eval (Receipt(..))
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.ContentStore

-- | 'ContentStore' for 'Receipt's where a receipt is adressed by the
-- hash of the transaction the receipt is for.
type ReceiptStore c tx o m = ContentStore (Crypto.Hashed c tx) (Receipt c tx o) m

-- | Underlying data structure for pure versions of 'ReceiptStore'
-- created with 'mkStateReceiptStore'.
type ReceiptMap c tx o = Map (Crypto.Hashed c tx) (Receipt c tx o)

newReceiptStoreIO
    :: (MonadIO m, Crypto.HasHashing c)
    => IO (ReceiptStore c tx o m)
newReceiptStoreIO = newContentStoreIO receiptTx

mkStateReceiptStore
    :: (MonadState state m, Crypto.HasHashing c)
    => Lens' state (ReceiptMap c tx o)
    -> ReceiptStore c tx o m
mkStateReceiptStore l = mkStateContentStore l receiptTx


class (Monad m) => MonadReceiptStore c tx o m | m -> tx o where
    getReceiptStore :: m (ReceiptStore c tx o m)

    default getReceiptStore
        :: (MonadReceiptStore c tx o m', MonadTrans t, m ~ t m')
        => m (ReceiptStore c tx o m)
    getReceiptStore = hoistContentStore lift <$> lift getReceiptStore


addReceipt :: (MonadReceiptStore c tx o m) => Receipt c tx o -> m ()
addReceipt receipt = do
    cs <- getReceiptStore
    storeContent cs receipt

lookupReceipt :: (MonadReceiptStore c tx o m) => Crypto.Hashed c tx -> m (Maybe (Receipt c tx o))
lookupReceipt hash = do
    cs <- getReceiptStore
    lookupContent cs hash
