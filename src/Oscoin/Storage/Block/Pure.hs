module Oscoin.Storage.Block.Pure
    ( Handle(..)
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
data Handle tx s = Handle
    { bsChains  :: Map BlockHash (Blockchain tx s) -- ^ Chains leading back to genesis.
    , bsOrphans :: Map BlockHash (Block tx s)      -- ^ Orphan blocks.
    , bsScoreFn :: ScoringFunction tx s            -- ^ Chain scoring function.
    }

instance Show (Handle tx s) where
    -- We can't derive Show because 'bsScoreFn' is a function.
    show = const "Handle{}"

instance (Ord tx, Ord s) => Semigroup (Handle tx s) where
    (<>) a b = Handle
        { bsOrphans = bsOrphans a <> bsOrphans b
        , bsChains  = bsChains  a <> bsChains  b
        , bsScoreFn = bsScoreFn a }

instance (Ord tx, Ord s) => Monoid (Handle tx s) where
    mempty = Handle mempty mempty (comparing height)


genesisBlockStore :: Block tx s -> Handle tx s
genesisBlockStore gen = initWithChain $ fromGenesis gen

initWithChain :: Blockchain tx s -> Handle tx s
initWithChain  chain =
    Handle
        { bsChains  = Map.singleton (blockHash $ genesis chain) chain
        , bsOrphans = mempty
        , bsScoreFn = comparing height
        }

getBlocks
    :: Depth
    -> Handle tx s
    -> [Block tx s]
getBlocks (fromIntegral -> d) Handle{..} =
    takeBlocks d . maximumBy bsScoreFn . Map.elems $ bsChains
-- Nb. we guarantee that there is at least one chain in the store by exposing
-- only the 'genesisHandle' smart constructor.

getTip :: Handle tx s -> Block tx s
getTip = tip . getBestChain

getBestChain :: Handle tx s -> Blockchain tx s
getBestChain Handle{..} =
    maximumBy bsScoreFn . Map.elems $ bsChains

-- | /O(n)/. Get the genesis block.
getGenesisBlock :: Handle tx s -> Block tx s
getGenesisBlock Handle{bsChains} =
    genesis (snd $ Map.findMin bsChains)
    -- Nb. since all blockchains share the same genesis block, we can just pick
    -- any.

insert :: Block tx s -> Handle tx s -> Handle tx s
insert blk bs@Handle{..} =
    linkBlocks $ bs { bsOrphans = Map.insert (blockHash blk) blk bsOrphans }

fromOrphans :: (Foldable t) => t (Block tx s) -> Block tx s -> Handle tx s
fromOrphans (toList -> blks) gen =
    linkBlocks $ (genesisBlockStore gen) { bsOrphans = Map.fromList [(blockHash b, b) |b <- blks] }

-- | /O(n)/. Lookup a block in all chains.
lookupBlock :: BlockHash -> Handle tx s -> Maybe (Block tx s)
lookupBlock h Handle{..} =
        Map.lookup h bsOrphans
    <|> List.find ((== h) . blockHash) (foldMap blocks $ Map.elems bsChains)

orphans :: Handle tx s -> Set BlockHash
orphans Handle{bsOrphans} =
    let parentHashes   = Set.fromList $ map (blockPrevHash . blockHeader) (Map.elems bsOrphans)
        danglingHashes = Map.keysSet bsOrphans
     in Set.difference parentHashes danglingHashes

-- | Lookup a transaction in the 'Handle'. Only considers transactions in
-- the best chain.
lookupTx :: forall tx s. (Hashable tx) => Hashed tx -> Handle tx s -> Maybe (TxLookup tx)
lookupTx h = Blockchain.lookupTx h . getBestChain

-- | The state hash of the chain.
chainStateHash :: Handle tx s -> StateHash
chainStateHash =
    blockStateHash . blockHeader . getTip

-- | Link as many orphans as possible to one of the existing chains. If the
-- linking of an orphan to its parent fails, the block is discarded.
linkBlocks :: Handle tx s -> Handle tx s
linkBlocks bs' =
    go (Map.elems (bsOrphans bs')) bs'
  where
    go [] bs =
        bs
    go (blk:blks) bs@Handle{bsChains, bsOrphans} =
        case Map.lookup (blockPrevHash (blockHeader blk)) bsChains of
            Just chain ->
                let store = Map.delete (blockHash blk) bsOrphans
                 in go (Map.elems store) $
                    bs { bsOrphans  = store
                       , bsChains   = Map.insert (blockHash blk)
                                                 (blk |> chain)
                                                 bsChains }
            Nothing ->
                go blks bs
