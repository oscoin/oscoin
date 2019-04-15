-- | A reference implementation of a 'BlockTree', suitable for model checking.

module Oscoin.Storage.Block.BlockTree.Reference
    ( newBlockTree
    ) where

import           Oscoin.Prelude

import           Oscoin.Storage.Block.Abstract (BlockStore)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.Block.BlockTree

newBlockTree
    :: ( Monad m )
    => BlockStore c tx s m
    -> BlockTree c tx s m
newBlockTree bs =
    let btree = BlockTree
            { btFullBlockStore = bs
            -- Inserting an orphan is indistinguishable from inserting a
            -- \"normal\" block, for the pure 'BlockTree'.
            , insertOrphan = \blk -> do
                BlockStore.insertBlock (snd bs) blk
                pure $ btree
            -- The pure 'BlockTree' searches automatically both in the main
            -- chain /and/ in the orphans.
            , isNovelBlock = \h -> not <$> BlockStore.isNovelBlock (fst bs) h
            }
    in btree
