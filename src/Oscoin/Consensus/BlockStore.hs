module Oscoin.Consensus.BlockStore
    ( BlockStore
    , bsChains
    , bsDangling

    , genesisBlockStore

    , maximumChainBy
    , insert
    , lookupBlock
    , lookupTx
    , orphans
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain(..), blockHash, tip, (|>))
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash

import qualified Data.Map as Map
import qualified Data.Set as Set

data BlockStore tx = BlockStore
    { bsChains   :: Map (Hashed BlockHeader) (Blockchain tx)
    , bsDangling :: Set (Block tx)
    } deriving (Eq, Show)

genesisBlockStore :: Ord tx => Block tx -> BlockStore tx
genesisBlockStore gen =
    BlockStore
        { bsChains   = Map.singleton (blockHash gen) (Blockchain (gen :| []))
        , bsDangling = mempty
        }

maximumChainBy
    :: (Blockchain tx -> Blockchain tx -> Ordering)
    -> BlockStore tx
    -> Blockchain tx
maximumChainBy cmp = maximumBy cmp . Map.elems . bsChains
-- Nb. we guarantee that there is at least one chain in the store by exposing
-- only the 'genesisBlockStore' smart constructor.

insert :: Ord tx => Block tx -> BlockStore tx -> BlockStore tx
insert blk bs@BlockStore{..} =
    constructChains $ bs { bsDangling = Set.insert blk bsDangling }

lookupBlock :: Hashed BlockHeader -> BlockStore tx -> Maybe (Block tx)
lookupBlock hdr = map tip . Map.lookup hdr . bsChains

orphans :: BlockStore tx -> Set (Hashed BlockHeader)
orphans BlockStore{bsDangling} =
    let parentHashes   = Set.map (blockPrevHash . blockHeader) bsDangling
        danglingHashes = Set.map blockHash bsDangling
     in Set.difference parentHashes danglingHashes

lookupTx :: forall tx. Hashable tx => Hashed tx -> BlockStore tx -> Maybe tx
lookupTx h BlockStore{..} =
    let txs :: [(Hashed tx, tx)]
        txs = [(hash tx, tx) | tx <- concatMap toList (Map.elems bsChains)]
     in lookup h txs

constructChains :: Ord tx => BlockStore tx -> BlockStore tx
constructChains n =
    go (Set.elems (bsDangling n)) n
  where
    go [] node =
        node
    go (blk:blks) bs@BlockStore{bsChains, bsDangling} =
        case Map.lookup (blockPrevHash (blockHeader blk)) bsChains of
            Just chain ->
                let store = Set.delete blk bsDangling
                 in go (Set.elems store) bs
                     { bsDangling  = store
                     , bsChains    = Map.insert (blockHash blk)
                                                (blk |> chain)
                                                bsChains }
            Nothing ->
                go blks bs
