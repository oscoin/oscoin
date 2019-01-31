module Oscoin.Storage.Block.Pure
    ( Handle
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
    { hChains  :: Map BlockHash (Blockchain tx s) -- ^ Chains leading back to genesis.
    , hOrphans :: Map BlockHash (Block tx s)      -- ^ Orphan blocks.
    , hScoreFn :: ScoringFunction tx s            -- ^ Chain scoring function.
    }

instance Show (Handle tx s) where
    -- We can't derive Show because 'hScoreFn' is a function.
    show = const "Handle{}"

instance (Ord tx, Ord s) => Semigroup (Handle tx s) where
    (<>) a b = Handle
        { hOrphans = hOrphans a <> hOrphans b
        , hChains  = hChains  a <> hChains  b
        , hScoreFn = hScoreFn a }

instance (Ord tx, Ord s) => Monoid (Handle tx s) where
    mempty = Handle mempty mempty (comparing height)


genesisBlockStore :: Block tx s -> Handle tx s
genesisBlockStore gen = initWithChain $ fromGenesis gen

initWithChain :: Blockchain tx s -> Handle tx s
initWithChain  chain =
    Handle
        { hChains  = Map.singleton (blockHash $ genesis chain) chain
        , hOrphans = mempty
        , hScoreFn = comparing height
        }

getBlocks
    :: Depth
    -> Handle tx s
    -> [Block tx s]
getBlocks (fromIntegral -> d) Handle{..} =
    takeBlocks d . maximumBy hScoreFn . Map.elems $ hChains
-- Nb. we guarantee that there is at least one chain in the store by exposing
-- only the 'genesisHandle' smart constructor.

getTip :: Handle tx s -> Block tx s
getTip = tip . getBestChain

getBestChain :: Handle tx s -> Blockchain tx s
getBestChain Handle{..} =
    maximumBy hScoreFn . Map.elems $ hChains

-- | /O(n)/. Get the genesis block.
getGenesisBlock :: Handle tx s -> Block tx s
getGenesisBlock Handle{hChains} =
    genesis (snd $ Map.findMin hChains)
    -- Nb. since all blockchains share the same genesis block, we can just pick
    -- any.

insert :: Block tx s -> Handle tx s -> Handle tx s
insert blk bs@Handle{..} =
    linkBlocks $ bs { hOrphans = Map.insert (blockHash blk) blk hOrphans }

fromOrphans :: (Foldable t) => t (Block tx s) -> Block tx s -> Handle tx s
fromOrphans (toList -> blks) gen =
    linkBlocks $ (genesisBlockStore gen) { hOrphans = Map.fromList [(blockHash b, b) |b <- blks] }

-- | /O(n)/. Lookup a block in all chains.
lookupBlock :: BlockHash -> Handle tx s -> Maybe (Block tx s)
lookupBlock h Handle{..} =
        Map.lookup h hOrphans
    <|> List.find ((== h) . blockHash) (foldMap blocks $ Map.elems hChains)

orphans :: Handle tx s -> Set BlockHash
orphans Handle{hOrphans} =
    let parentHashes   = Set.fromList $ map (blockPrevHash . blockHeader) (Map.elems hOrphans)
        danglingHashes = Map.keysSet hOrphans
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
    go (Map.elems (hOrphans bs')) bs'
  where
    go [] bs =
        bs
    go (blk:blks) bs@Handle{hChains, hOrphans} =
        case Map.lookup (blockPrevHash (blockHeader blk)) hChains of
            Just chain ->
                let store = Map.delete (blockHash blk) hOrphans
                 in go (Map.elems store) $
                    bs { hOrphans  = store
                       , hChains   = Map.insert (blockHash blk)
                                                 (blk |> chain)
                                                 hChains }
            Nothing ->
                go blks bs
