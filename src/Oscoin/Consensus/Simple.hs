-- TODO: When committing a block, filter the mempool of all transactions in that
-- block.
module Oscoin.Consensus.Simple where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class
import           Oscoin.Consensus.BlockStore
import           Oscoin.Crypto.Blockchain (Blockchain(..), blockHash, tip, height)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash

import qualified Data.ByteString.Char8 as C8
import           Data.Binary (Binary)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Network.Socket

epochLength :: Tick
epochLength = 1

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
    deriving (Eq, Generic)

instance Binary tx => Binary (NodeMsg tx)

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

addPeer :: SockAddr -> SimpleNode tx -> SimpleNode tx
addPeer addr sn =
    sn { snPeers = peers' }
  where
    peers  = snPeers sn
    peers' = if addr == snAddr sn
        then peers
        else Set.toList $ Set.insert addr $ Set.fromList peers

applyBlock :: Ord tx => Block tx -> SimpleNode tx -> SimpleNode tx
applyBlock blk sn@SimpleNode{..} =
    sn { snStore = storeBlock blk snStore }

isNovelTx :: Ord tx => tx -> SimpleNode tx -> Bool
isNovelTx tx SimpleNode { snBuffer } =
    not inBuffer
  where
    inBuffer = Set.member tx snBuffer

bestChain :: BlockStore tx -> Blockchain tx
bestChain BlockStore{bsChains} =
    Blockchain longest
  where
    scored = [(chainScore chain, fromBlockchain chain) | chain <- toList bsChains]
    (_, longest) = maximumBy (comparing fst) scored

chainScore :: forall tx . Blockchain tx -> Int
chainScore bc =
    (bigMagicNumber * h) - steps
  where
    h              = height bc
    lastBlock      = tip bc :: Block tx
    timestampNs    = blockTimestamp $ blockHeader lastBlock
    timestamp      = timestampNs `div` 1000000000000
    e              = round epochLength
    steps          = fromIntegral timestamp `div` e :: Int
    bigMagicNumber = 2526041640 -- some loser in 2050 has to deal with this bug

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
    type Addr (SimpleNode tx) = SockAddr

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

    epoch _ = epochLength
