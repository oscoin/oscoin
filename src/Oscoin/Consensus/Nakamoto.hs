module Oscoin.Consensus.Nakamoto where

import           Oscoin.Prelude

import           Oscoin.Consensus.Class
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashed, Hashable, hash, shortHash)
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool

import           Crypto.Number.Serialize (os2ip)
import           Data.Binary
import qualified Data.ByteString.Char8 as C8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Map as Map

type BlockHash = Hashed BlockHeader

data Nakamoto tx = Nakamoto
    { nkForks   :: Map BlockHash (Blockchain tx)
    , nkStore   :: Set (Block tx)
    , nkRandom  :: [Float]
    , nkMempool :: Mempool (Hashed tx) tx
    , nkPeers   :: Set (Addr (Nakamoto tx))
    , nkAddr    :: Addr (Nakamoto tx)
    }

instance Show (Nakamoto tx) where
    show Nakamoto{} = "Nakamoto{}"

nakamoto :: Ord tx => Addr (Nakamoto tx) -> Block tx -> [Addr (Nakamoto tx)] -> [Float] -> Nakamoto tx
nakamoto addr genesis peers random =
    Nakamoto
        { nkForks = Map.singleton (blockHash genesis) chain
        , nkStore = mempty
        , nkRandom = random
        , nkMempool = mempty
        , nkPeers = Set.fromList peers
        , nkAddr = addr
        }
  where
    chain = Blockchain (genesis :| [])

data NodeMsg tx =
      BlockMsg   (Block tx)
    | TxMsg      tx
    deriving (Eq, Generic)

instance Show tx => Show (NodeMsg tx) where
    show (BlockMsg blk) =
        unwords [ "BlockMsg"
                , C8.unpack (shortHash (blockHash blk))
                , show (toList (blockData blk)) ]
    show (TxMsg tx) =
        "TxMsg " ++ show tx

instance Binary tx => Binary (NodeMsg tx)

instance (Ord tx, Hashable tx) => Protocol (Nakamoto tx) where
    type Addr (Nakamoto tx) = Word8
    type Msg  (Nakamoto tx) = NodeMsg tx

    step :: Nakamoto tx
         -> Tick
         -> Maybe (Addr (Nakamoto tx), NodeMsg tx)
         -> (Nakamoto tx, [(Addr (Nakamoto tx), NodeMsg tx)])
    step node _ (Just (_from, msg)) =
        case msg of
            BlockMsg blk | Right _ <- validateBlock blk ->
                (commitBlock blk node, [])
            TxMsg tx | isNovel tx node -> -- TODO: Validate tx.
                -- NB. Normally, the node would flood the network with the
                -- transaction, but this slows things down considerably
                -- for our tests.
                (receiveTx tx node, [])
            _ ->
                (node, [])
    step node t Nothing =
        mineBlock t node

    epoch _ = 1

isNovel :: Hashable tx => tx -> Nakamoto tx -> Bool
isNovel tx Nakamoto{nkMempool} =
    not $ Mempool.member (hash tx) nkMempool

broadcast :: Foldable t => msg -> t peer -> [(peer, msg)]
broadcast msg peers = zip (toList peers) (repeat msg)

receiveTx :: Hashable tx => tx -> Nakamoto tx -> Nakamoto tx
receiveTx tx node = node { nkMempool = Mempool.insert tx (nkMempool node) }

commitBlock :: (Ord tx, Hashable tx) => Block tx -> Nakamoto tx -> Nakamoto tx
commitBlock blk =
    reapMempool (toList blk) . storeBlock blk

storeBlock :: Ord tx => Block tx -> Nakamoto tx -> Nakamoto tx
storeBlock blk node@Nakamoto{nkStore} =
    constructChains $ node { nkStore = Set.insert blk nkStore }

longestChain :: Nakamoto tx -> Blockchain tx
longestChain = maximumBy (comparing height) . Map.elems . nkForks

reapMempool :: (Functor t, Foldable t, Hashable tx) => t tx -> Nakamoto tx -> Nakamoto tx
reapMempool txs node =
    node { nkMempool = Mempool.removeTxs (map hash txs) (nkMempool node) }

constructChains :: Ord tx => Nakamoto tx -> Nakamoto tx
constructChains n =
    go (Set.elems (nkStore n)) n
  where
    go [] node =
        node
    go (blk:blks) node =
        case Map.lookup (blockPrevHash (blockHeader blk)) (nkForks node) of
            Just chain ->
                let store = Set.delete blk (nkStore node)
                 in go (Set.elems store) node
                     { nkStore  = store
                     , nkForks  = Map.insert (blockHash blk)
                                             (blk |> chain)
                                             (nkForks node) }
            Nothing ->
                go blks node

mineBlock
    :: (Ord tx, Hashable tx)
    => Tick
    -> Nakamoto tx
    -> (Nakamoto tx, [(Addr (Nakamoto tx), NodeMsg tx)])
mineBlock t node@Nakamoto{nkRandom, nkMempool, nkPeers}
    | head nkRandom < 0.1 =
        case findBlock t (hash . blockHeader . tip $ longestChain node) minDifficulty nkMempool of
            Just blk ->
                (commitBlock blk node', broadcast (BlockMsg blk) (toList nkPeers))
            Nothing ->
                (node', [])
    | otherwise =
        (node', [])
  where
    node' = node { nkRandom = tail nkRandom }

-- | Calculate block difficulty.
difficulty :: BlockHeader -> Difficulty
difficulty = os2ip . hash

-- | The minimum difficulty.
minDifficulty :: Difficulty
minDifficulty =
    0xEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | The default difficulty at genesis.
defaultGenesisDifficulty :: Difficulty
defaultGenesisDifficulty =
    0x00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    -- ^ This is the original difficulty of Bitcoin at genesis.

-- | Return whether or not a block has a valid difficulty.
hasPoW :: BlockHeader -> Bool
hasPoW header =
    difficulty header < blockDifficulty header

-- | Calculate the difficulty of a blockchain.
chainDifficulty :: Blockchain tx -> Difficulty
chainDifficulty (Blockchain blks) =
    if   length range < blocksConsidered
    then genesisDifficulty
    else currentDifficulty * targetElapsed `div` toInteger actualElapsed
  where
    range             = NonEmpty.take blocksConsidered blks
    rangeStart        = blockHeader $ NonEmpty.last (NonEmpty.head blks :| tail range)
    rangeEnd          = blockHeader $ NonEmpty.head blks
    actualElapsed     = blockTimestamp rangeEnd - blockTimestamp rangeStart
    targetElapsed     = fromIntegral $ blocksConsidered * blockTimeSeconds
    blocksConsidered  = timePeriodMinutes `div` blockTimeMinutes
    timePeriodMinutes = timePeriodDays * 24 * 60
    timePeriodDays    = 2 * 7 -- Two weeks.
    blockTimeMinutes  = 10
    blockTimeSeconds  = blockTimeMinutes * 60
    currentDifficulty = blockDifficulty rangeEnd
    genesisDifficulty = blockDifficulty . blockHeader $ NonEmpty.last blks

findPoW :: BlockHeader -> Maybe BlockHeader
findPoW bh@BlockHeader { blockNonce }
    | hasPoW bh =
        Just bh
    | blockNonce < (maxBound :: Word32) =
        findPoW bh { blockNonce = blockNonce + 1 }
    | otherwise =
        Nothing

findBlock
    :: (Binary tx, Foldable t)
    => Tick
    -> Hashed BlockHeader
    -> Difficulty
    -> t tx
    -> Maybe (Block tx)
findBlock t prevHash target txs = do
    header <- headerWithPoW
    pure $ mkBlock header txs
  where
    headerWithPoW    = findPoW headerWithoutPoW
    headerWithoutPoW = BlockHeader
        { blockPrevHash     = prevHash
        , blockRootHash     = hashTxs txs
        , blockDifficulty   = target
        , blockTimestamp    = toSeconds t
        , blockNonce        = 0
        }
    toSeconds = fromIntegral . fromEnum
