-- TODO: When committing a block, filter the mempool of all transactions in that
-- block.
module Oscoin.Consensus.Simple where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class
import           Oscoin.Crypto.Blockchain (Blockchain(..), blockHash, tip)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash

import qualified Data.ByteString.Char8 as C8
import           Data.Binary (Binary)
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
    deriving (Eq)

instance Show tx => Show (NodeMsg tx) where
    show (BroadcastBlock blk) =
        unwords [ "BroadcastBlock"
                , C8.unpack (shortHash (blockHash blk))
                , show (toList (blockData blk)) ]
    show (ClientTx tx) =
        "ClientTx " ++ show tx
    show (BlockAtHeight h blk) =
        "BlockAtHeight " ++ show h ++ C8.unpack (shortHash (blockHash blk))
    show (RequestBlockAtHeight h) =
        "RequestBlockAtHeight " ++ show h

data BlockStore tx = BlockStore
    { bsChains   :: Map (Hashed BlockHeader) (Blockchain tx)
    , bsDangling :: Set (Block tx)
    } deriving (Eq, Show)

emptyBlockStore :: BlockStore tx
emptyBlockStore = BlockStore mempty Set.empty

genesisBlockStore :: Binary tx => BlockStore tx
genesisBlockStore = BlockStore (Map.fromList [(blockHash gen, Blockchain (gen :| []))]) Set.empty
  where
    gen = genesisBlock 0 []

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
        Nothing                 -> Nothing
        Just (Blockchain chain) -> Just $
            bs { bsDangling = Set.delete blk bsDangling
               , bsChains   = Map.insert (blockHash blk) (Blockchain $ blk <| chain) bsChains
               }

applyBlock :: Ord tx => Block tx -> SimpleNode tx -> SimpleNode tx
applyBlock blk sn@SimpleNode{..} =
    sn { snStore = (applyDanglings . addToDangling blk) snStore }

isNovelTx :: Ord tx => tx -> SimpleNode tx -> Bool
isNovelTx tx SimpleNode { snBuffer } =
    not inBuffer
  where
    inBuffer = Set.member tx snBuffer

bestChain :: BlockStore tx -> Blockchain tx
bestChain BlockStore{bsChains} =
    Blockchain longest
  where
    scored = [(length chain, fromBlockchain chain) | chain <- toList bsChains]
    (_, longest) = maximumBy (comparing fst) scored

chainTxs :: Blockchain tx -> [tx]
chainTxs (Blockchain chain) =
    concat blocks
  where
    blocks = map (toList . blockData) $ toList chain

offset :: SimpleNode tx -> Int
offset SimpleNode{..} =
    length peersLtUs
  where
    peersLtUs = filter (< snAddr) snPeers

shouldCutBlock :: (Ord tx, Binary tx, Show tx) => SimpleNode tx -> Tick -> Bool
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

instance (Binary tx, Ord tx, Show tx) => Protocol (SimpleNode tx) where
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
                (applyBlock blk sn, [])
    step sn@SimpleNode{..} _ (Just (_, BlockAtHeight _h _b)) =
        (sn, [])
    step sn@SimpleNode{..} _ (Just (_, RequestBlockAtHeight _h)) =
        (sn, [])

    step sn@SimpleNode{..} tick Nothing
        | shouldCutBlock sn tick =
            (applyBlock blk $ sn { snTick = tick, snBuffer = mempty }, outgoing)
        | otherwise =
            (sn, [])
      where
        chain      = bestChain snStore
        lastBlock  = tip chain
        lastHeader = blockHeader lastBlock
        parentHash = hash lastHeader
        blk        = block parentHash 0 snBuffer
        msg        = BroadcastBlock blk
        outgoing   = [(p, msg) | p <- snPeers]

    epoch _ = 10
