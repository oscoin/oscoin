-- | 'ContentStore' is an interface for a mutable key-value store where
-- the key is computed from the value.
module Oscoin.Storage.ContentStore
    ( ContentStore
    , storeContent
    , lookupContent
    , hoistContentStore

    , newContentStoreIO
    , mkStateContentStore

    ) where

import           Oscoin.Prelude

import qualified Data.Map as Map
import           Lens.Micro
import           Lens.Micro.Mtl

data ContentStore key value m = ContentStore
    { _storeContent  :: value -> m ()
    , _lookupContent :: key -> m (Maybe value)
    }

storeContent :: ContentStore k v m -> v -> m ()
storeContent = _storeContent

lookupContent :: ContentStore k v m -> k -> m (Maybe v)
lookupContent = _lookupContent

hoistContentStore :: (forall x. m x -> n x) -> ContentStore k v m -> ContentStore k v n
hoistContentStore natTrans c = ContentStore
    { _storeContent = natTrans . _storeContent c
    , _lookupContent = natTrans . _lookupContent c
    }

------------------------------------------------------------
-- * Constructors
------------------------------------------------------------


-- | Create an empty 'ContentStore' that uses a 'Map' in an 'MVar'
-- under the hood
newContentStoreIO
    :: (MonadIO m, Ord k)
    => (v -> k)
    -> IO (ContentStore k v m)
newContentStoreIO keyForContent = do
    mapVar <- newMVar mempty
    let _lookupContent k = liftIO $ readMVar mapVar <&> Map.lookup k
    let _storeContent v = liftIO $ modifyMVar_ mapVar $ \m -> pure $ Map.insert (keyForContent v) v m
    pure $ ContentStore{_storeContent, _lookupContent}


-- | Create a 'ContentStore' that modifies the 'Map' in the state when
-- an item is stored. The map that holds the content is accessed though
-- the provided lens.
mkStateContentStore
    :: (MonadState state m, Ord k)
    => Lens' state (Map k v)
    -> (v -> k)
    -> ContentStore k v m
mkStateContentStore mapL keyForContent =
    ContentStore{_storeContent, _lookupContent}
  where
    _lookupContent k = do
        m <- use mapL
        pure $ Map.lookup k m
    _storeContent v =
        mapL %= Map.insert (keyForContent v) v
