module Oscoin.Storage.Block
    ( BlockStore(..)
    , genesisBlockStore
    , fromOrphans
    , initWithChain

    , maximumChainBy
    , getGenesisBlock
    , insert
    , lookupBlock
    , lookupTx
    , orphans
    , chainStateHash
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (Block)

import           Oscoin.Crypto.Blockchain hiding (lookupTx)
import           Oscoin.Crypto.Hash (Hashable, Hashed, hash)

import           Codec.Serialise (Serialise)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Text.Show (Show(..))

-- | Store of 'Block's and 'Blockchain's.
data BlockStore tx s = BlockStore
    { bsChains  :: Map BlockHash (Blockchain tx s) -- ^ Chains leading back to genesis.
    , bsOrphans :: Set (Block tx s)                -- ^ Orphan blocks.
    }

instance Show (BlockStore tx s) where
    -- We can't derive Show because 'Orphan' is a function.
    show = const "BlockStore{}"

instance (Ord tx, Ord s) => Semigroup (BlockStore tx s) where
    (<>) a b = BlockStore
        { bsOrphans = bsOrphans a <> bsOrphans b
        , bsChains  = bsChains  a <> bsChains  b }

instance (Ord tx, Ord s) => Monoid (BlockStore tx s) where
    mempty = BlockStore mempty mempty

genesisBlockStore :: Serialise s => Block tx s -> BlockStore tx s
genesisBlockStore gen = initWithChain $ fromGenesis gen

initWithChain :: Serialise s => Blockchain tx s -> BlockStore tx s
initWithChain  chain =
    BlockStore
        { bsChains  = Map.singleton (blockHash $ genesis chain) chain
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

insert :: (Ord s, Ord tx, Serialise s) => Block tx s -> BlockStore tx s -> BlockStore tx s
insert blk bs@BlockStore{..} =
    linkBlocks $ bs { bsOrphans = Set.insert blk bsOrphans }

fromOrphans :: (Ord tx, Ord s, Serialise s, Foldable t) => t (Block tx s) -> Block tx s -> BlockStore tx s
fromOrphans (toList -> blks) gen =
    linkBlocks $ (genesisBlockStore gen) { bsOrphans = Set.fromList blks }

-- | /O(n)/. Lookup a block in all chains.
lookupBlock :: Serialise s => BlockHash -> BlockStore tx s -> Maybe (Block tx s)
lookupBlock h (Map.elems . bsChains -> chains) =
    List.find ((== h) . blockHash) (foldMap blocks chains)

orphans :: Serialise s => BlockStore tx s -> Set BlockHash
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
     in List.lookup h txs

-- | The state hash of the best chain according to the supplied scoring function.
chainStateHash :: ScoringFunction tx s -> BlockStore tx s -> StateHash
chainStateHash sf =
    blockStateHash . blockHeader . tip . maximumChainBy sf

-- | Link as many orphans as possible to one of the existing chains. If the
-- linking of an orphan to its parent fails, the block is discarded.
linkBlocks :: forall tx s. (Ord tx, Ord s, Serialise s) => BlockStore tx s -> BlockStore tx s
linkBlocks bs' =
    go (Set.elems (bsOrphans bs')) bs'
  where
    go [] bs =
        bs
    go (blk:blks) bs@BlockStore{bsChains, bsOrphans} =
        case Map.lookup (blockPrevHash (blockHeader blk)) bsChains of
            Just chain ->
                let store = Set.delete blk bsOrphans
                 in go (Set.elems store) $
                    bs { bsOrphans  = store
                       , bsChains   = Map.insert (blockHash blk)
                                                 (blk |> chain)
                                                 bsChains }
            Nothing ->
                go blks bs
