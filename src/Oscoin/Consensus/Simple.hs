-- TODO: When committing a block, filter the mempool of all transactions in that
-- block.
module Oscoin.Consensus.Simple where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class
import           Oscoin.Crypto.Blockchain (Blockchain, blockHash)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash

import           Data.Binary (Binary)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty ((<|))
import qualified Data.Set as Set
import qualified Data.Map as Map

data SimpleNode tx = SimpleNode
    { snAddr    :: Addr (SimpleNode tx)
    , snPeers   :: [Addr (SimpleNode tx)]
    , snBuffer  :: Set tx
    , snTick    :: Tick
    , snStore   :: BlockStore tx
    } deriving (Eq, Show)

type BlockId = Word64
type Step = Word64
type Score = Word64

data NodeMsg tx =
      BroadcastBlock              (Block tx)
    | BlockAtHeight        Height (Block tx)
    | RequestBlockAtHeight Height
    | ClientTx tx
    deriving (Eq, Show)

data BlockStore tx = BlockStore
    { bsChains   :: Map (Hashed BlockHeader) (Blockchain tx)
    , bsDangling :: Set (Block tx)
    } deriving (Eq, Show)

emptyBlockStore :: BlockStore tx
emptyBlockStore = BlockStore mempty Set.empty

addToDangling :: Ord tx => Block tx -> BlockStore tx -> BlockStore tx
addToDangling blk bs@BlockStore{..} =
    bs { bsDangling = Set.insert blk bsDangling }

applyDanglings :: Ord tx => BlockStore tx -> BlockStore tx
applyDanglings bs@BlockStore{..} =
    go (Set.toList bsDangling) bs
  where
    go []         bs = bs
    go (blk:blks) bs =
        case applyDangling blk bs of
            Nothing  -> go blks bs
            Just bs' -> go blks bs'

applyDangling :: Ord tx => Block tx -> BlockStore tx -> Maybe (BlockStore tx)
applyDangling blk@Block{blockHeader} bs@BlockStore{..} =
    case Map.lookup (blockPrevHash blockHeader) bsChains of
        Nothing    -> Nothing
        Just chain -> Just $
            bs { bsDangling = Set.delete blk bsDangling
               , bsChains   = Map.insert (blockHash blk) (blk <| chain) bsChains
               }

isNovelTx :: Ord tx => tx -> SimpleNode tx -> Bool
isNovelTx tx SimpleNode { snBuffer } =
    not inBuffer
  where
    inBuffer = Set.member tx snBuffer

bestChain :: Binary tx => BlockStore tx -> Blockchain tx
bestChain BlockStore { bsChains } =
    longestChain
  where
    scored = [(length chain, chain) | chain <- toList bsChains]
    genesis = (1, NonEmpty.fromList [genesisBlock 0 []])
    (_, longestChain) =
        foldl' (\(accLen, accChain) (chainLen, chain) ->
            if chainLen > accLen
                then (chainLen, chain)
                else (accLen, accChain))
            genesis
            scored

chainTxs :: Blockchain tx -> [tx]
chainTxs chain =
    concat blocks
  where
    blocks = map (toList . blockData) $ toList chain

offset :: SimpleNode tx -> Int
offset SimpleNode{..} =
    length peersLtUs
  where
    peersLtUs = filter (< snAddr) snPeers

shouldCutBlock :: (Ord tx, Binary tx) => SimpleNode tx -> Tick -> Bool
shouldCutBlock sn@SimpleNode{..} at =
    beenAWhile && ourTurn
  where
    time              = round at
    lastTick          = round snTick
    stepTime          = round (epoch sn)
    nTotalPeers       = 1 + length snPeers
    relativeBlockTime = stepTime * nTotalPeers
    beenAWhile        = time - lastTick >= relativeBlockTime
    step              = time `div` stepTime
    ourOffset         = offset sn
    currentOffset     = step `mod` nTotalPeers
    ourTurn           = currentOffset == ourOffset

instance (Binary tx, Ord tx) => Protocol (SimpleNode tx) where
    type Msg  (SimpleNode tx) = NodeMsg tx
    type Addr (SimpleNode tx) = Word8

    step sn@SimpleNode{..} _ (Just (_, ClientTx msg))
        | isNovelTx msg sn =
            (sn { snBuffer = Set.insert msg snBuffer }, [])
        | otherwise =
            (sn, [])
    step sn@SimpleNode{..} _ (Just (_, BroadcastBlock blk)) =
        case validateBlock blk of
            Left _ ->
                (sn, [])
            Right _ ->
                (sn { snStore = (applyDanglings . addToDangling blk) snStore }, [])
    step sn@SimpleNode{..} _ (Just (_, BlockAtHeight _h _b)) =
        (sn, [])
    step sn@SimpleNode{..} _ (Just (_, RequestBlockAtHeight _h)) =
        (sn, [])

    step sn@SimpleNode{..} tick Nothing
        | shouldCutBlock sn tick =
            (sn { snTick = tick, snBuffer = mempty }, outgoing)
        | otherwise =
            (sn, [])
      where
        chain      = bestChain snStore
        height     = 1 + length chain
        lastBlock  = NonEmpty.head chain
        lastHeader = blockHeader lastBlock
        parentHash = hash lastHeader
        msg        = BroadcastBlock $ block parentHash 0 snBuffer
        outgoing   = [(p, msg) | p <- snPeers]

    epoch _ = 10
