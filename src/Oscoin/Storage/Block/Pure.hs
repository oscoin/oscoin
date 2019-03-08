{- | A pure, in-memory storage for a 'BlockChain', suitable to be used as
   a concrete implementation for a 'BlockStore'.
-}
{-# LANGUAGE UndecidableInstances #-}
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

import           Oscoin.Consensus.Types (ChainScoreFn)
import           Oscoin.Crypto.Blockchain hiding (lookupTx)
import qualified Oscoin.Crypto.Blockchain as Blockchain
import           Oscoin.Crypto.Blockchain.Block (Block)
import           Oscoin.Crypto.Hash (Hash, Hashable, Hashed)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Text.Show (Show(..))

-- | Store of 'Block's and 'Blockchain's.
data Handle c tx s = Handle
    { hChains  :: Map (BlockHash c) (Blockchain c tx s)       -- ^ Chains leading back to genesis.
    , hOrphans :: Map (BlockHash c) (Block c tx (Sealed c s)) -- ^ Orphan blocks.
    , hScoreFn :: ChainScoreFn c tx s                         -- ^ Chain scoring function.
    }

instance Show (Handle c tx s) where
    -- We can't derive Show because 'hScoreFn' is a function.
    show = const "Handle{}"

instance (Ord tx, Ord (Hash c), Ord s) => Semigroup (Handle c tx s) where
    (<>) a b = Handle
        { hOrphans = hOrphans a <> hOrphans b
        , hChains  = hChains  a <> hChains  b
        , hScoreFn = hScoreFn a }

instance (Ord tx, Ord (Hash c), Ord s) => Monoid (Handle c tx s) where
    mempty = Handle mempty mempty (comparing height)


genesisBlockStore
    :: Ord (Hash c)
    => Block c tx (Sealed c s)
    -> ScoreFn c tx (Sealed c s)
    -> Handle c tx s
genesisBlockStore gen sf = initWithChain (fromGenesis gen) sf

-- | Like 'initWithChain', but allows the user to override the stock
-- 'ScoringFunction', expressed in terms of a function at the block-level.
initWithChain
    :: Ord (Hash c)
    => Blockchain c tx s
    -> ScoreFn c tx (Sealed c s)
    -- ^ A function to score blocks. The use of the existential is because
    -- /we do not care/ about the seal when scoring blocks together.
    -> Handle c tx s
initWithChain chain scoreFn =
    Handle
        { hChains  = Map.singleton (blockHash $ genesis chain) chain
        , hOrphans = mempty
        , hScoreFn = compareChainsDefault scoreFn
        }

compareChainsDefault :: Ord (BlockHash c)
                     => ScoreFn c tx (Sealed c s)
                     -- ^ A function to score blocks. The use of the existential is because
                     -- /we do not care/ about the seal when scoring blocks together.
                     -> Blockchain c tx s
                     -> Blockchain c tx s
                     -> Ordering
compareChainsDefault scoreBlock c1 c2 =
    let compareScore   = chainScore c1 `compare` chainScore c2
        compareHeight  = height c1 `compare` height c2
        compareTipHash = (blockHash . tip $ c1) `compare` (blockHash . tip $ c2)
    -- If we score a draw, pick the longest.
    in mconcat [compareScore, compareHeight, compareTipHash]
  where
      chainScore = sum . map scoreBlock . blocks

getBlocks
    :: Depth
    -> Handle c tx s
    -> [Block c tx (Sealed c s)]
getBlocks (fromIntegral -> d) Handle{..} =
    takeBlocks d . maximumBy hScoreFn . Map.elems $ hChains
-- Nb. we guarantee that there is at least one chain in the store by exposing
-- only the 'genesisHandle' smart constructor.

getTip :: Handle c tx s -> Block c tx (Sealed c s)
getTip = tip . getBestChain

getBestChain :: Handle c tx s -> Blockchain c tx s
getBestChain Handle{..} =
    maximumBy hScoreFn . Map.elems $ hChains

-- | /O(n)/. Get the genesis block.
getGenesisBlock :: Handle c tx s -> Block c tx (Sealed c s)
getGenesisBlock Handle{hChains} =
    genesis (snd $ Map.findMin hChains)
    -- Nb. since all blockchains share the same genesis block, we can just pick
    -- any.

insert
    :: Ord (BlockHash c)
    => Block c tx (Sealed c s)
    -> Handle c tx s
    -> Handle c tx s
insert blk bs@Handle{..} =
    linkBlocks $ bs { hOrphans = Map.insert (blockHash blk) blk hOrphans }

fromOrphans
    :: (Foldable t, Ord (Hash c))
    => t (Block c tx (Sealed c s))
    -> Block c tx (Sealed c s)
    -> ScoreFn c tx (Sealed c s)
    -> Handle c tx s
fromOrphans (toList -> blks) gen sf =
    linkBlocks $ (genesisBlockStore gen sf) { hOrphans = Map.fromList [(blockHash b, b) |b <- blks] }

-- | /O(n)/. Lookup a block in all chains.
lookupBlock
    :: Ord (BlockHash c)
    => BlockHash c
    -> Handle c tx s
    -> Maybe (Block c tx (Sealed c s))
lookupBlock h Handle{..} =
        Map.lookup h hOrphans
    <|> List.find ((== h) . blockHash) (foldMap blocks $ Map.elems hChains)

orphans :: Ord (Hash c) => Handle c tx s -> Set (BlockHash c)
orphans Handle{hOrphans} =
    let parentHashes   = Set.fromList $ map (blockPrevHash . blockHeader) (Map.elems hOrphans)
        danglingHashes = Map.keysSet hOrphans
     in Set.difference parentHashes danglingHashes

-- | Lookup a transaction in the 'Handle'. Only considers transactions in
-- the best chain.
lookupTx
    :: forall c tx s. (Eq (Hash c), Hashable c tx)
    => Hashed c tx
    -> Handle c tx s
    -> Maybe (TxLookup c tx)
lookupTx h = Blockchain.lookupTx h . getBestChain

-- | The state hash of the chain.
chainStateHash :: Handle c tx s -> StateHash c
chainStateHash =
    blockStateHash . blockHeader . getTip

-- | Link as many orphans as possible to one of the existing chains. If the
-- linking of an orphan to its parent fails, the block is discarded.
linkBlocks :: Ord (Hash c) => Handle c tx s -> Handle c tx s
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
