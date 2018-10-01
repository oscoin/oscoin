-- | Provides heleprs functions to easily implement 'MonadReceiptStore'
-- instances for State monads and IO Reader monads with handles.
module Oscoin.Storage.Receipt
    ( MonadReceiptStore(..)
    , Receipt(..)

    -- * State based receipt store
    , ReceiptStore
    , HasReceiptStore(..)
    , emptyReceiptStore
    , addReceiptStore
    , lookupReceiptStore

    -- * TVar handle based receipt store
    , ReceiptStoreHandle
    , HasReceiptStoreHandle(..)
    , newReceiptHandle
    , addReceiptHandle
    , lookupReceiptHandle
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Eval (Receipt(..))
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Receipt.Class

import           Control.Concurrent.STM
import qualified Data.Map as Map
import           Lens.Micro
import           Lens.Micro.Mtl

-- State based MonadReceiptStore --------------------------------------

type ReceiptStore tx o = Map.Map (Crypto.Hashed tx) (Receipt tx o)

class HasReceiptStore tx o r where
    receiptStoreL :: Lens' r (ReceiptStore tx o)

emptyReceiptStore :: ReceiptStore tx o
emptyReceiptStore = Map.empty

addReceiptStore
    :: (HasReceiptStore tx o st, MonadState st m)
    => Receipt tx o -> m ()
addReceiptStore receipt = do
    store <- use receiptStoreL
    let store' = Map.insert (receiptTx receipt) receipt store
    receiptStoreL .= store'

lookupReceiptStore
    :: (HasReceiptStore tx o st, MonadState st m)
    => Crypto.Hashed tx -> m (Maybe (Receipt tx o))
lookupReceiptStore txHash = do
    store <- use receiptStoreL
    pure $ Map.lookup txHash store

-- TVar handle based MonadReceiptStore --------------------------------------

type ReceiptStoreHandle tx o = TVar (ReceiptStore tx o)

class HasReceiptStoreHandle tx o r where
    receiptStoreHandleL :: Lens' r (ReceiptStoreHandle tx o)

newReceiptHandle :: MonadIO m => m (ReceiptStoreHandle o tx)
newReceiptHandle = liftIO $ newTVarIO emptyReceiptStore

addReceiptHandle
    :: (HasReceiptStoreHandle tx o r, MonadReader r m, MonadIO m)
    => Receipt tx o -> m ()
addReceiptHandle receipt = do
    store <- view receiptStoreHandleL
    liftIO $ atomically $ modifyTVar' store $ Map.insert (receiptTx receipt) receipt

lookupReceiptHandle
    :: (HasReceiptStoreHandle tx o r, MonadReader r m, MonadIO m)
    => Crypto.Hashed tx -> m (Maybe (Receipt tx o))
lookupReceiptHandle txHash = do
    store <- view receiptStoreHandleL
    Map.lookup txHash <$> liftIO (readTVarIO store)
