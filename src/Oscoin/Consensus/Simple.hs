module Oscoin.Consensus.Simple where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class
import           Oscoin.Crypto.Blockchain (Blockchain)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash


import           Data.Binary (Binary)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           Data.Time.Clock (NominalDiffTime)

data SimpleNode tx = SimpleNode
    { snAddr    :: Addr (SimpleNode tx)
    , snPeers   :: [Addr (SimpleNode tx)]
    , snBuffer  :: Set tx
    , snTick    :: Tick (SimpleNode tx)
    , snStore   :: BlockStore tx
    } deriving (Eq, Show)

type BlockId = Word64
type Step = Word64
type Score = Word64

data NodeMsg tx =
      BroadcastBlock (Block tx)
    | RequestBlockAtHeight Height
    | BlockAtHeight (Block tx)
    | ClientTx tx
    deriving (Eq, Show)

data BlockStore tx = BlockStore
    { bsChains   :: Map (Hashed BlockId) (Blockchain tx)
    , bsDangling :: Set (Block tx)
    } deriving (Eq, Show)

emptyBlockStore :: BlockStore tx
emptyBlockStore = BlockStore mempty Set.empty

isNovelTx :: (Binary tx, Ord tx) => tx -> SimpleNode tx -> Bool
isNovelTx tx SimpleNode { snBuffer, snStore } =
    not inBuffer && not inChains
  where
    inBuffer = Set.member tx snBuffer
    bc = bestChain snStore
    ctxs = chainTxs bc
    setTxs = Set.fromList ctxs
    inChains = Set.member tx setTxs

bestChain :: Binary tx => BlockStore tx -> Blockchain tx
bestChain BlockStore { bsChains } =
    longestChain
  where
    scored = [(length chain, chain) | chain <- toList bsChains]
    genesis = (1, NonEmpty.fromList [genesisBlock 0 []])
    (_, longestChain) =
        foldl (\(accLen, accChain) (chainLen, chain) ->
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

shouldCutBlock :: (Ord tx, Binary tx) => SimpleNode tx -> Tick (SimpleNode tx) -> Bool
shouldCutBlock sn@SimpleNode{..} at =
    beenAWhile && ourTurn
  where
    time = fromIntegral $ round at :: Int
    lastTick = fromIntegral $ round snTick :: Int
    stepTime = fromIntegral . round $ epoch sn :: Int
    nTotalPeers = 1 + length snPeers
    relativeBlockTime = stepTime * nTotalPeers
    beenAWhile = (time - lastTick) >= relativeBlockTime
    step = time `div` stepTime
    ourOffset = offset sn
    currentOffset = step `mod` nTotalPeers
    ourTurn = currentOffset == ourOffset

instance (Binary tx, Ord tx) => Protocol (SimpleNode tx) where
    type Msg  (SimpleNode tx) = NodeMsg tx
    type Addr (SimpleNode tx) = Word8
    type Tick (SimpleNode tx) = NominalDiffTime

    step sn@SimpleNode{..} _ (Just (_, ClientTx msg))
        | isNovelTx msg sn =
            (sn { snBuffer = Set.insert msg snBuffer }, [])
        | otherwise =
            (sn, [])
    step sn@SimpleNode{..} _ (Just (_, BroadcastBlock _)) =
        (sn, [])
    -- TODO(tyler): Fill in other types of messages.
    step sn@SimpleNode{..} _ (Just (_, _)) = (sn, [])

    step sn@SimpleNode{..} tick Nothing
        | shouldCutBlock sn tick =
            (sn { snTick = tick, snBuffer = mempty }, outgoing)
        | otherwise =
            (sn, [])
      where
        chain = bestChain snStore
        height = 1 + length chain
        lastBlock = NonEmpty.head chain
        lastHeader = blockHeader lastBlock
        parentHash = hash lastHeader
        b = block parentHash 0 snBuffer :: Block tx
        msg = BroadcastBlock b :: NodeMsg tx
        outgoing = [(p, msg) | p <- snPeers]

    epoch _ = 10