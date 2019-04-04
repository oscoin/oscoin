-- | 'HashStore' is an interface for storing values adressed by
-- their hashes. Conceptually 'HashStore' is a mutable 'Map'
-- where the key for each value is determined by @'Crypto.hash' value@.
module Oscoin.Storage.HashStore
    ( HashStore
    , storeHashContent
    , lookupHashContent

    , newHashStoreIO
    , mkStateHashStore
    , hoistHashStore
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto

import qualified Data.Map.Strict as Map
import           Lens.Micro
import           Lens.Micro.Mtl

data HashStore crypto value m = HashStore
    { _storeHashContent  :: value -> m ()
    , _lookupHashContent :: Crypto.Hashed crypto value -> m (Maybe value)
    }

storeHashContent :: HashStore crypto a m -> a -> m ()
storeHashContent = _storeHashContent

lookupHashContent :: HashStore crypto a m -> Crypto.Hashed crypto a -> m (Maybe a)
lookupHashContent = _lookupHashContent

hoistHashStore
    :: (forall x. m x -> n x)
    -> HashStore crypto a m
    -> HashStore crypto a n
hoistHashStore natTrans c = HashStore
    { _storeHashContent = natTrans . storeHashContent c
    , _lookupHashContent = natTrans . lookupHashContent c
    }


-- | Creates 'HashStore' that uses a 'Map' in an 'MVar' under
-- the hood
newHashStoreIO
    :: forall crypto a m.
       ( MonadIO m
       , Crypto.Hashable crypto a
       )
    => IO (HashStore crypto a m)
newHashStoreIO = do
    mapVar <- newMVar mempty
    let _lookupHashContent hash = liftIO $ readMVar mapVar <&> Map.lookup hash
    let _storeHashContent a = liftIO $ modifyMVar_ mapVar $ \m -> pure $ Map.insert (Crypto.hash @crypto a) a m
    pure $ HashStore{_storeHashContent, _lookupHashContent}


-- | Creates 'HashStore' that modifies the 'Map' in the state
-- when an item is stored. The map that holds the content is accessed
-- though the provided lens.
mkStateHashStore
    :: forall crypto state a m.
       ( MonadState state m
       , Crypto.Hashable crypto a)
    => Lens' state (Map (Crypto.Hashed crypto a) a)
    -> HashStore crypto a m
mkStateHashStore mapL =
    HashStore{_storeHashContent, _lookupHashContent}
  where
    _lookupHashContent hash = do
        m <- use mapL
        pure $ Map.lookup hash m
    _storeHashContent a = mapL %= Map.insert (Crypto.hash a) a
