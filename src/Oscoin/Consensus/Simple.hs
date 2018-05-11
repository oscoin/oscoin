-- TODO: When committing a block, filter the mempool of all transactions in that
-- block.
module Oscoin.Consensus.Simple where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class
import           Oscoin.Crypto.Blockchain (Blockchain(..), blockHash, tip, height)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash

import qualified Data.ByteString.Char8 as C8
import           Data.Binary (Binary)
import           Data.List.NonEmpty ((<|))
import qualified Data.Set as Set
import qualified Data.Map as Map

epochLen :: Tick
epochLen = 10

data SimpleNode tx = SimpleNode
    { snAddr    :: Addr (SimpleNode tx)
    , snPeers   :: [Addr (SimpleNode tx)]
    , snBuffer  :: Set tx
    , snLastBlk :: Tick
    , snLastAsk :: Tick
    , snStore   :: BlockStore tx
    } deriving (Eq, Show)

type BlockId = Word64
type Step = Word64
type Score = Word64

data NodeMsg tx =
      SendBlock                   (Block tx)
    | ProposeBlock                (Block tx)
    | RequestBlock                (Hashed BlockHeader)
    | ClientTx tx
    deriving (Eq)

instance Show tx => Show (NodeMsg tx) where
    show (ProposeBlock blk) =
        unwords [ "ProposeBlock"
                , C8.unpack (shortHash (blockHash blk))
                , show (toList (blockData blk)) ]
    show (SendBlock blk) =
        unwords [ "SendBlock"
                , C8.unpack (shortHash (blockHash blk))
                , show (toList (blockData blk)) ]
    show (ClientTx tx) =
        "ClientTx " ++ show tx
    show (RequestBlock h) =
        "RequestBlock " ++ C8.unpack (shortHash h)

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
applyDanglings blockStore =
    go (Set.toList (bsDangling blockStore)) blockStore
  where
    go []                            bs                = bs -- Nothing dangling, do nothing.
    go (blk@Block{blockHeader}:blks) bs@BlockStore{..} =    -- Something's dangling.
        case Map.lookup (blockPrevHash blockHeader) bsChains of
            Nothing ->                 -- The dangler has no known parent.
                go blks bs
            Just (Blockchain chain) -> -- The dangler has a parent.
                let dangling = Set.delete blk bsDangling
                 in go (Set.toList dangling) bs
                     { bsDangling = dangling
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
    scored = [(scoreChain chain, fromBlockchain chain) | chain <- toList bsChains]
    (_, longest) = maximumBy (comparing fst) scored

scoreChain :: forall tx . Blockchain tx -> Int
scoreChain bc =
    (2526041640 * h) - stepDuration
  where
    h            = height bc
    lastBlock    = tip bc :: Block tx
    timestampNs  = blockTimestamp $ blockHeader lastBlock
    timestamp    = timestampNs `div` 1000000000
    e            = round epochLen
    stepDuration = fromIntegral timestamp `div` e :: Int

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

lookupBlock :: Hashed BlockHeader -> SimpleNode tx -> Maybe (Block tx)
lookupBlock header SimpleNode{..} =
    map tip chain
  where
    chains = bsChains snStore
    chain = Map.lookup header chains

shouldReconcile :: (Ord tx, Binary tx, Show tx) => SimpleNode tx -> Tick -> Bool
shouldReconcile sn@SimpleNode { snLastAsk } at =
    time - lastTick >= stepTime
  where
    time     = round at
    lastTick = round snLastAsk
    stepTime = round (epoch sn) :: Int

shouldCutBlock :: (Ord tx, Binary tx, Show tx) => SimpleNode tx -> Tick -> Bool
shouldCutBlock sn@SimpleNode{..} at =
    beenAWhile && ourTurn
  where
    time              = round at
    lastTick          = round snLastBlk
    stepTime          = round (epoch sn)
    nTotalPeers       = 1 + length snPeers
    relativeBlockTime = stepTime * nTotalPeers
    beenAWhile        = time - lastTick >= relativeBlockTime
    stepNumber        = time `div` stepTime
    ourOffset         = offset sn
    currentOffset     = stepNumber `mod` nTotalPeers
    ourTurn           = currentOffset == ourOffset

orphanParentHashes :: SimpleNode tx -> [Hashed BlockHeader]
orphanParentHashes SimpleNode{..} =
    orphanHashes
  where
    dangling        = bsDangling snStore
    danglingHashes  = Set.map blockHash dangling
    parentHashes    = Set.map (blockPrevHash . blockHeader) dangling
    orphanHashes    = Set.toList $ Set.difference parentHashes danglingHashes

instance (Binary tx, Ord tx, Show tx) => Protocol (SimpleNode tx) where
    type Msg  (SimpleNode tx) = NodeMsg tx
    type Addr (SimpleNode tx) = Word8

    step sn@SimpleNode{..} _ (Just (_, ClientTx msg))
        | isNovelTx msg sn =
            (sn { snBuffer = Set.insert msg snBuffer }, [])
        | otherwise =
            (sn, [])
    step sn@SimpleNode{..} _ (Just (_, SendBlock blk)) =
        case validateBlock blk of
            Left _ ->
                (sn, [])
            Right _ ->
                (applyBlock blk sn, [])
    step sn t (Just (from, ProposeBlock blk)) =
        step sn t (Just (from, SendBlock blk))

    step sn@SimpleNode{..} _ (Just (requestor, RequestBlock blkHash)) =
        case lookupBlock blkHash sn of
            Just blk -> (sn, [(requestor, SendBlock blk)])
            Nothing  -> (sn, [])
    step sn@SimpleNode{..} tick Nothing
        | shouldCutBlock sn tick =
            (applyBlock blk $ sn { snLastBlk = tick, snBuffer = mempty }, outgoingBlk)
        | shouldReconcile sn tick =
            (sn { snLastAsk = tick }, outgoingAsks)
        | otherwise =
            (sn, [])
      where
        chain         = bestChain snStore
        lastBlock     = tip chain
        lastHeader    = blockHeader lastBlock
        parentHash    = hash lastHeader
        timestamp     = fromIntegral (fromEnum tick)
        blk           = block parentHash timestamp snBuffer
        msg           = ProposeBlock blk
        outgoingBlk   = [(p, msg) | p <- snPeers]
        orphanParents = orphanParentHashes sn
        outgoingAsks  = [(p, RequestBlock orphan) | p <- snPeers, orphan <- orphanParents]

    epoch _ = epochLen
