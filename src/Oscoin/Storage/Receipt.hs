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
import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Receipt.Class

import           Control.Concurrent.STM
import qualified Data.Map as Map
import           Lens.Micro
import           Lens.Micro.Mtl

-- State based MonadReceiptStore --------------------------------------

type Store c tx o = Map.Map (Crypto.Hashed c tx) (Receipt c tx o)

class HasStore c tx o r where
    storeL :: Lens' r (Store c tx o)

emptyStore :: Store c tx o
emptyStore = Map.empty

addWithStore
    :: (Ord (Hash c), HasStore c tx o st, MonadState st m)
    => Receipt c tx o -> m ()
addWithStore receipt = do
    store <- use storeL
    let store' = Map.insert (receiptTx receipt) receipt store
    storeL .= store'

lookupWithStore
    :: (Ord (Hash c), HasStore c tx o st, MonadState st m)
    => Crypto.Hashed c tx -> m (Maybe (Receipt c tx o))
lookupWithStore txHash =
    Map.lookup txHash <$> use storeL

-- TVar handle based MonadReceiptStore --------------------------------------

type Handle c tx o = TVar (Store c tx o)

class HasHandle c tx o r where
    handleL :: Lens' r (Handle c tx o)

newHandle :: MonadIO m => m (Handle c o tx)
newHandle = liftIO $ newTVarIO emptyStore

addWithHandle
    :: (Ord (Hash c), HasHandle c tx o r, MonadReader r m, MonadIO m)
    => Receipt c tx o -> m ()
addWithHandle receipt = do
    store <- view handleL
    liftIO $ atomically $ modifyTVar' store $ Map.insert (receiptTx receipt) receipt

lookupWithHandle
    :: (Ord (Hash c), HasHandle c tx o r, MonadReader r m, MonadIO m)
    => Crypto.Hashed c tx -> m (Maybe (Receipt c tx o))
lookupWithHandle txHash = do
    store <- view handleL
    Map.lookup txHash <$> liftIO (readTVarIO store)
