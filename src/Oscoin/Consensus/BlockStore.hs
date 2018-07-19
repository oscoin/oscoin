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
import           Oscoin.Crypto.Hash

import qualified Data.Map as Map
import qualified Data.Set as Set

-- TODO(alexis): Document fields and 'Orphan'.
data BlockStore tx s = BlockStore
    { bsChains  :: Map (Hashed (BlockHeader ())) (Blockchain tx s)
    , bsOrphans :: Set (Block tx (Orphan s))
    }

instance Show tx => Show (BlockStore tx s) where
    -- TODO(alexis): Fix this.
    show BlockStore{..} = "BlockStore{..}"

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

lookupBlock :: Hashed (BlockHeader ()) -> BlockStore tx s -> Maybe (Block tx s)
lookupBlock hdr = map tip . Map.lookup hdr . bsChains

orphans :: BlockStore tx s -> Set (Hashed (BlockHeader ()))
orphans BlockStore{bsOrphans} =
    let parentHashes   = Set.map (blockPrevHash . blockHeader) bsOrphans
        danglingHashes = Set.map blockHash bsOrphans
     in Set.difference parentHashes danglingHashes

constructChains :: forall tx s. Ord tx => BlockStore tx s -> BlockStore tx s
constructChains n =
    go (Set.elems (bsOrphans n)) n
  where
    go [] node =
        node
    go (blk:blks) bs@BlockStore{bsChains, bsOrphans} =
        case Map.lookup (blockPrevHash (blockHeader blk)) bsChains of
            Just chain ->
                let store = Set.delete blk bsOrphans
                    -- TODO(alexis): Handle 'Nothing' here.
                    blk'  = fromJust $ linkBlock (tip chain) blk
                 in go (Set.elems store) bs
                     { bsOrphans  = store
                     , bsChains    = Map.insert (blockHash blk')
                                                (blk' |> chain)
                                                bsChains }
            Nothing ->
                go blks bs
