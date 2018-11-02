-- | Provides helper functions to easily implement 'MonadReceiptStore'
-- instances for State monads and IO Reader monads with handles.
module Oscoin.Storage.Receipt
    ( MonadReceiptStore(..)
    , Receipt(..)

    -- * State based receipt store
    , Store
    , HasStore(..)
    , emptyStore
    , addWithStore
    , lookupWithStore

    -- * TVar handle based receipt store
    , Handle
    , HasHandle(..)
    , newHandle
    , addWithHandle
    , lookupWithHandle
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

type Store tx o = Map.Map (Crypto.Hashed tx) (Receipt tx o)

class HasStore tx o r where
    storeL :: Lens' r (Store tx o)

emptyStore :: Store tx o
emptyStore = Map.empty

addWithStore
    :: (HasStore tx o st, MonadState st m)
    => Receipt tx o -> m ()
addWithStore receipt = do
    store <- use storeL
    let store' = Map.insert (receiptTx receipt) receipt store
    storeL .= store'

lookupWithStore
    :: (HasStore tx o st, MonadState st m)
    => Crypto.Hashed tx -> m (Maybe (Receipt tx o))
lookupWithStore txHash =
    Map.lookup txHash <$> use storeL

-- TVar handle based MonadReceiptStore --------------------------------------

type Handle tx o = TVar (Store tx o)

class HasHandle tx o r where
    handleL :: Lens' r (Handle tx o)

newHandle :: MonadIO m => m (Handle o tx)
newHandle = liftIO $ newTVarIO emptyStore

addWithHandle
    :: (HasHandle tx o r, MonadReader r m, MonadIO m)
    => Receipt tx o -> m ()
addWithHandle receipt = do
    store <- view handleL
    liftIO $ atomically $ modifyTVar' store $ Map.insert (receiptTx receipt) receipt

lookupWithHandle
    :: (HasHandle tx o r, MonadReader r m, MonadIO m)
    => Crypto.Hashed tx -> m (Maybe (Receipt tx o))
lookupWithHandle txHash = do
    store <- view handleL
    Map.lookup txHash <$> liftIO (readTVarIO store)
