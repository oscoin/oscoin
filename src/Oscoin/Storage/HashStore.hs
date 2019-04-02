-- | 'HashStore' is a specialized version of 'ContentStore' that uses
-- 'Crypto.hash' as the key function.
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
import           Oscoin.Storage.ContentStore

import           Lens.Micro

type HashStore crypto a m = ContentStore (Crypto.Hashed crypto a) a m

hoistHashStore :: (forall x. m x -> n x) -> HashStore crypto a m -> HashStore crypto a n
hoistHashStore = hoistContentStore

storeHashContent :: HashStore crypto v m -> v -> m ()
storeHashContent = storeContent

lookupHashContent :: HashStore crypto v m -> Crypto.Hashed crypto v -> m (Maybe v)
lookupHashContent = lookupContent

-- | Creates 'HashStore' that uses a 'Map' in an 'MVar' under
-- the hood
newHashStoreIO
    :: forall crypto a m.
       ( MonadIO m
       , Crypto.Hashable crypto a
       )
    => IO (HashStore crypto a m)
newHashStoreIO = newContentStoreIO (Crypto.hash @crypto)

-- | Creates 'HashStore' that modifies the 'Map' in the state
-- when an item is stored. The map that holds the content is accessed
-- though the provided lens.
mkStateHashStore
    :: forall crypto state a m.
       ( MonadState state m
       , Crypto.Hashable crypto a)
    => Lens' state (Map (Crypto.Hashed crypto a) a)
    -> HashStore crypto a m
mkStateHashStore mapL = mkStateContentStore mapL (Crypto.hash @crypto)
