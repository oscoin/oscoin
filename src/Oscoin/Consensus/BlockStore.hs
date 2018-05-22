module Oscoin.Consensus.BlockStore
    ( BlockStore(..)
    , emptyBlockStore
    , genesisBlockStore
    , storeBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain(..), blockHash, (|>))
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash

import qualified Data.Map as Map
import qualified Data.Set as Set

data BlockStore tx = BlockStore
    { bsChains   :: Map (Hashed BlockHeader) (Blockchain tx)
    , bsDangling :: Set (Block tx)
    } deriving (Eq, Show)

emptyBlockStore :: BlockStore tx
emptyBlockStore = BlockStore mempty Set.empty

genesisBlockStore :: Ord tx => Block tx -> BlockStore tx
genesisBlockStore gen =
    BlockStore
        { bsChains   = Map.singleton (blockHash gen) (Blockchain (gen :| []))
        , bsDangling = mempty
        }

storeBlock :: Ord tx => Block tx -> BlockStore tx -> BlockStore tx
storeBlock blk bs@BlockStore{..} =
    constructChains $ bs { bsDangling = Set.insert blk bsDangling }

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
