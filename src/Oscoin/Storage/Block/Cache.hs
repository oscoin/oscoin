{-- | A cache for 'Blocks', implemented as a ring buffer.
 -- It caches the \"mutable\" part of the chain, which are usually the
 -- last @k@ blocks, where @k@ is a protocol constant.
 --}
module Oscoin.Storage.Block.Cache
    ( BlockCache
    , newBlockCache
    , consBlock
    , viewBlocks
    , invalidate
    , cached
    ) where

import           Oscoin.Prelude hiding (length)

import           GHC.Natural

import           Oscoin.Crypto.Blockchain.Block (Block)
import           Oscoin.Time.Chrono (NewestFirst(..))

import           Data.Sequence.Circular (CircularSeq)
import qualified Data.Sequence.Circular as CSeq

newtype BlockCache c tx s =
    BlockCache { getBlockCache :: MVar (CircularSeq (Block c tx s)) }

cached
    :: BlockCache c tx s
    -> Int
    -- ^ The number of requested elements
    -> (Int -> IO (NewestFirst [] (Block c tx s)))
    -- ^ A function to lookup the requested elements on disk, if the cache
    -- cannot serve all of them directly.
    -> IO (NewestFirst [] (Block c tx s))
cached (BlockCache bc) requestedElements lookupOnDisk =
    modifyMVar bc $ \cseq -> do
        let currentSize = CSeq.getSize cseq
        -- If the cache has enough elements, serve them.
        if currentSize < requestedElements
          then do
              -- Otherwise, hit the disk.
              newElements <- lookupOnDisk requestedElements
              pure (cseq, newElements)
          else pure (cseq, NewestFirst $ take requestedElements (CSeq.toList cseq))

newBlockCache :: Natural -> IO (BlockCache c tx s)
newBlockCache limit =
    BlockCache <$> newMVar (CSeq.empty limit)

-- | Adds a 'Block' to the front of the cache. If the limit is exceeded,
-- the oldest element is evicted.
consBlock :: BlockCache c tx s -> Block c tx s -> IO ()
consBlock cache blk =
    modifyMVar_ (getBlockCache cache) (pure . (CSeq.<|) blk)

viewBlocks :: BlockCache c tx s -> IO [Block c tx s]
viewBlocks cache = do
    cseq <- readMVar (getBlockCache cache)
    pure $ CSeq.toList cseq

-- | Invalidate the cache.
invalidate :: BlockCache c tx s -> IO ()
invalidate (BlockCache cacheVar) =
    modifyMVar_ cacheVar $ \old -> do
      let new = CSeq.empty (fromIntegral $ CSeq.getLimit old)
      pure new
