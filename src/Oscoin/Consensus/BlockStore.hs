module Oscoin.Consensus.BlockStore
    ( BlockStore
    , genesisBlockStore

    , maximumChainBy
    , insert
    , lookupBlock
    , orphans
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain(..), blockHash, tip, (|>))
import           Oscoin.Crypto.Blockchain.Block

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Store of 'Block's and 'Blockchain's.
data BlockStore tx s = BlockStore
    { bsChains  :: Map BlockHash (Blockchain tx s) -- ^ Chains leading back to genesis.
    , bsOrphans :: Set (Block tx (Orphan s))       -- ^ Orphan blocks.
    } deriving (Show)

genesisBlockStore :: Block tx s -> BlockStore tx s
genesisBlockStore gen =
    BlockStore
        { bsChains   = Map.singleton (blockHash gen) (Blockchain (gen :| []))
        , bsOrphans = Set.empty
        }

maximumChainBy
    :: (Blockchain tx s -> Blockchain tx s -> Ordering)
    -> BlockStore tx s
    -> Blockchain tx s
maximumChainBy cmp = maximumBy cmp . Map.elems . bsChains
-- Nb. we guarantee that there is at least one chain in the store by exposing
-- only the 'genesisBlockStore' smart constructor.

insert :: Ord tx => Block tx (Orphan s) -> BlockStore tx s -> BlockStore tx s
insert blk bs@BlockStore{..} =
    constructChains $ bs { bsOrphans = Set.insert blk bsOrphans }

lookupBlock :: BlockHash -> BlockStore tx s -> Maybe (Block tx s)
lookupBlock hdr = map tip . Map.lookup hdr . bsChains

orphans :: BlockStore tx s -> Set BlockHash
orphans BlockStore{bsOrphans} =
    let parentHashes   = Set.map (blockPrevHash . blockHeader) bsOrphans
        danglingHashes = Set.map blockHash bsOrphans
     in Set.difference parentHashes danglingHashes

constructChains :: forall tx s. Ord tx => BlockStore tx s -> BlockStore tx s
constructChains bs' =
    go (Set.elems (bsOrphans bs')) bs'
  where
    go [] bs =
        bs
    go (blk:blks) bs@BlockStore{bsChains, bsOrphans} =
        case Map.lookup (blockPrevHash (blockHeader blk)) bsChains of
            Just chain ->
                let store = Set.delete blk bsOrphans
                    mblk' = linkBlock (tip chain) blk
                 in go (Set.elems store) $ case mblk' of
                     Just blk' -> bs
                         { bsOrphans  = store
                         , bsChains    = Map.insert (blockHash blk')
                                                    (blk' |> chain)
                                                    bsChains }
                     Nothing -> bs
                         { bsOrphans = store }
            Nothing ->
                go blks bs
