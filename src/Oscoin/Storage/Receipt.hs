-- | This module provides the 'ReceiptStore' interface which is a
-- 'ContentStore' that uses the transaction hashes to identify
-- transaction receipts.
--
-- For legacy reasons we also provide the 'MonadReceiptStore' type
-- class to obtain a receipt store.
module Oscoin.Storage.Receipt
    ( ReceiptStore
    , storeReceipt
    , lookupReceipt
    , hoistReceiptStore

    , ReceiptMap
    , newReceiptStoreIO
    , mkStateReceiptStore
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

storeReceipt :: ReceiptStore c tx o m -> Receipt c tx o -> m ()
storeReceipt = storeContent

lookupReceipt :: ReceiptStore c tx o m -> Crypto.Hashed c tx -> m (Maybe (Receipt c tx o))
lookupReceipt = lookupContent

hoistReceiptStore :: (forall x. m x -> n x) -> ReceiptStore c tx o m -> ReceiptStore c tx o n
hoistReceiptStore = hoistContentStore

newReceiptStoreIO
    :: (MonadIO m, Crypto.HasHashing c)
    => IO (ReceiptStore c tx o m)
newReceiptStoreIO = newContentStoreIO receiptTx

mkStateReceiptStore
    :: (MonadState state m, Crypto.HasHashing c)
    => Lens' state (ReceiptMap c tx o)
    -> ReceiptStore c tx o m
mkStateReceiptStore l = mkStateContentStore l receiptTx
