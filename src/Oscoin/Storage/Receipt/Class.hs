-- | This module provides the 'MonadReceiptStore' type class to store
-- and lookup receipts.
--
-- Morally, a receipt store is a "Data.Map" storing 'Receipt' where
-- @'receiptTx' receipt@ is used as the key.
--
-- Implementations can be found in "Oscoin.Storage.Receipt".
module Oscoin.Storage.Receipt.Class
    ( MonadReceiptStore(..)
    ) where

import           Oscoin.Crypto.Blockchain.Eval (Receipt(..))
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Prelude

class (Monad m) => MonadReceiptStore tx o m | m -> tx o where
    addReceipt :: Receipt tx o -> m ()
    lookupReceipt :: Crypto.Hashed tx -> m (Maybe (Receipt tx o))

    default addReceipt
        :: (MonadReceiptStore tx o m', MonadTrans t, m ~ t m')
        => Receipt tx o -> m ()
    addReceipt = lift . addReceipt

    default lookupReceipt
        :: (MonadReceiptStore tx o m', MonadTrans t, m ~ t m')
        => Crypto.Hashed tx -> m (Maybe (Receipt tx o))
    lookupReceipt = lift . lookupReceipt
