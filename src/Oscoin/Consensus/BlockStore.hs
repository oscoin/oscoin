module Oscoin.Consensus.BlockStore
    ( BlockStore(..)
    , genesisBlockStore
    , fromOrphans

    , maximumChainBy
    , getGenesisBlock
    , insert
    , lookupBlock
    , lookupTx
    , orphans
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
                 (Blockchain(..), blockHash, blocks, genesis, tip, (|>))
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashable, Hashed, hash)

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Store of 'Block's and 'Blockchain's.
data BlockStore tx s = BlockStore
    { bsChains  :: Map BlockHash (Blockchain tx s) -- ^ Chains leading back to genesis.
    , bsOrphans :: Set (Block tx (Orphan s))       -- ^ Orphan blocks.
    }

instance Show (BlockStore tx s) where
    -- We can't derive Show because 'Orphan' is a function.
    show = const "BlockStore{}"

instance Ord tx => Semigroup (BlockStore tx s) where
    (<>) a b = BlockStore
        { bsOrphans = bsOrphans a <> bsOrphans b
        , bsChains  = bsChains  a <> bsChains  b }

instance Ord tx => Monoid (BlockStore tx s) where
    mempty = BlockStore mempty mempty

genesisBlockStore :: Block tx s -> BlockStore tx s
genesisBlockStore gen =
    BlockStore
        { bsChains  = Map.singleton (blockHash gen) (Blockchain (gen :| []))
        , bsOrphans = Set.empty
        }

maximumChainBy
    :: (Blockchain tx s -> Blockchain tx s -> Ordering)
    -> BlockStore tx s
    -> Blockchain tx s
maximumChainBy cmp = maximumBy cmp . Map.elems . bsChains
-- Nb. we guarantee that there is at least one chain in the store by exposing
-- only the 'genesisBlockStore' smart constructor.

-- | /O(n)/. Get the genesis block.
getGenesisBlock :: BlockStore tx s -> Block tx s
getGenesisBlock BlockStore{bsChains} =
    genesis (snd $ Map.findMin bsChains)
    -- Nb. since all blockchains share the same genesis block, we can just pick
    -- any.

insert :: Ord tx => Block tx (Orphan s) -> BlockStore tx s -> BlockStore tx s
insert blk bs@BlockStore{..} =
    linkBlocks $ bs { bsOrphans = Set.insert blk bsOrphans }

fromOrphans :: (Ord tx, Foldable t) => t (Block tx (Orphan s)) -> Block tx s -> BlockStore tx s
fromOrphans (toList -> blks) gen =
    linkBlocks $ (genesisBlockStore gen) { bsOrphans = Set.fromList blks }

lookupBlock :: BlockHash -> BlockStore tx s -> Maybe (Block tx s)
lookupBlock hdr = map tip . Map.lookup hdr . bsChains

orphans :: BlockStore tx s -> Set BlockHash
orphans BlockStore{bsOrphans} =
    let parentHashes   = Set.map (blockPrevHash . blockHeader) bsOrphans
        danglingHashes = Set.map blockHash bsOrphans
     in Set.difference parentHashes danglingHashes

-- | Lookup a transaction in the 'BlockStore'. Only considers transactions in
-- blocks which lead back to genesis.
lookupTx :: forall tx s. Hashable tx => Hashed tx -> BlockStore tx s -> Maybe tx
lookupTx h BlockStore{bsChains} =
    -- Nb. This is very slow. One way to make it faster would be to traverse
    -- all chains starting from the tip, in lock step, because it's likely
    -- that the transaction we're looking for is near the tip.
    let txs  = [(hash tx, tx) | tx <- concatMap (toList . blockData) blks]
        blks = concatMap blocks (Map.elems bsChains)
     in lookup h txs

-- | Link as many orphans as possible to one of the existing chains. If the
-- linking of an orphan to its parent fails, the block is discarded.
linkBlocks :: forall tx s. Ord tx => BlockStore tx s -> BlockStore tx s
linkBlocks bs' =
    go (Set.elems (bsOrphans bs')) bs'
  where
    go [] bs =
        bs
    go (blk:blks) bs@BlockStore{bsChains, bsOrphans} =
        case Map.lookup (blockPrevHash (blockHeader blk)) bsChains of
            Just chain ->
                let store = Set.delete blk bsOrphans
                 in go (Set.elems store) $ case linkBlock (tip chain) blk of
                     Just blk' -> bs
                         { bsOrphans  = store
                         , bsChains    = Map.insert (blockHash blk')
                                                    (blk' |> chain)
                                                    bsChains }
                     Nothing -> bs
                         { bsOrphans = store }
            Nothing ->
                go blks bs
