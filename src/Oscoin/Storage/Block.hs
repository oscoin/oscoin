module Oscoin.Storage.Block
    ( BlockStore(..)
    , genesisBlockStore
    , fromOrphans
    , initWithChain

    , getBlocks
    , getTip
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
import qualified Oscoin.Crypto.Blockchain as Blockchain
import           Oscoin.Crypto.Hash (Hashable, Hashed)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Text.Show (Show(..))

-- | Store of 'Block's and 'Blockchain's.
data BlockStore tx s = BlockStore
    { bsChains  :: Map BlockHash (Blockchain tx s) -- ^ Chains leading back to genesis.
    , bsOrphans :: Set (Block tx s)                -- ^ Orphan blocks.
    , bsScoreFn :: ScoringFunction tx s            -- ^ Chain scoring function.
    }

instance Show (BlockStore tx s) where
    -- We can't derive Show because 'bsScoreFn' is a function.
    show = const "BlockStore{}"

instance (Ord tx, Ord s) => Semigroup (BlockStore tx s) where
    (<>) a b = BlockStore
        { bsOrphans = bsOrphans a <> bsOrphans b
        , bsChains  = bsChains  a <> bsChains  b
        , bsScoreFn = bsScoreFn a }

instance (Ord tx, Ord s) => Monoid (BlockStore tx s) where
    mempty = BlockStore mempty mempty (comparing height)

genesisBlockStore :: Block tx s -> BlockStore tx s
genesisBlockStore gen = initWithChain $ fromGenesis gen

initWithChain :: Blockchain tx s -> BlockStore tx s
initWithChain  chain =
    BlockStore
        { bsChains  = Map.singleton (blockHash $ genesis chain) chain
        , bsOrphans = Set.empty
        , bsScoreFn = comparing height
        }

getBlocks
    :: Depth
    -> BlockStore tx s
    -> [Block tx s]
getBlocks (fromIntegral -> d) BlockStore{..} =
    takeBlocks d . maximumBy bsScoreFn . Map.elems $ bsChains
-- Nb. we guarantee that there is at least one chain in the store by exposing
-- only the 'genesisBlockStore' smart constructor.

getTip :: BlockStore tx s -> Block tx s
getTip = tip . getBestChain

getBestChain :: BlockStore tx s -> Blockchain tx s
getBestChain BlockStore{..} =
    maximumBy bsScoreFn . Map.elems $ bsChains

-- | /O(n)/. Get the genesis block.
getGenesisBlock :: BlockStore tx s -> Block tx s
getGenesisBlock BlockStore{bsChains} =
    genesis (snd $ Map.findMin bsChains)
    -- Nb. since all blockchains share the same genesis block, we can just pick
    -- any.

insert :: (Ord s, Ord tx) => Block tx s -> BlockStore tx s -> BlockStore tx s
insert blk bs@BlockStore{..} =
    linkBlocks $ bs { bsOrphans = Set.insert blk bsOrphans }

fromOrphans :: (Ord tx, Ord s, Foldable t) => t (Block tx s) -> Block tx s -> BlockStore tx s
fromOrphans (toList -> blks) gen =
    linkBlocks $ (genesisBlockStore gen) { bsOrphans = Set.fromList blks }

-- | /O(n)/. Lookup a block in all chains.
lookupBlock :: BlockHash -> BlockStore tx s -> Maybe (Block tx s)
lookupBlock h (Map.elems . bsChains -> chains) =
    List.find ((== h) . blockHash) (foldMap blocks chains)

orphans :: BlockStore tx s -> Set BlockHash
orphans BlockStore{bsOrphans} =
    let parentHashes   = Set.map (blockPrevHash . blockHeader) bsOrphans
        danglingHashes = Set.map blockHash bsOrphans
     in Set.difference parentHashes danglingHashes

-- | Lookup a transaction in the 'BlockStore'. Only considers transactions in
-- the best chain.
lookupTx :: forall tx s. (Hashable tx) => Hashed tx -> BlockStore tx s -> Maybe (TxLookup tx)
lookupTx h = Blockchain.lookupTx h . getBestChain

-- | The state hash of the chain.
chainStateHash :: BlockStore tx s -> StateHash
chainStateHash =
    blockStateHash . blockHeader . getTip

-- | Link as many orphans as possible to one of the existing chains. If the
-- linking of an orphan to its parent fails, the block is discarded.
linkBlocks :: forall tx s. (Ord tx, Ord s) => BlockStore tx s -> BlockStore tx s
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
