module Oscoin.Consensus.Nakamoto where

import           Oscoin.Prelude

import           Oscoin.Consensus.Class
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashed, Hashable, hash)
import           Oscoin.Node.Mempool (Mempool)
import qualified Oscoin.Node.Mempool as Mempool

import           Crypto.Number.Serialize (os2ip)
import           Data.Binary
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

data Nakamoto tx = Nakamoto
    { nkChain   :: Blockchain tx
    , nkRandom  :: [Float]
    , nkMempool :: Mempool (Hashed tx) tx
    , nkPeers   :: Set (Addr (Nakamoto tx))
    , nkAddr    :: Addr (Nakamoto tx)
    }

instance Show (Nakamoto tx) where
    show Nakamoto{} = "Nakamoto{}"

nakamoto :: Addr (Nakamoto tx) -> Block tx -> [Addr (Nakamoto tx)] -> [Float] -> Nakamoto tx
nakamoto addr genesis peers random =
    Nakamoto
        { nkChain = Blockchain (genesis :| [])
        , nkRandom = random
        , nkMempool = mempty
        , nkPeers = Set.fromList peers
        , nkAddr = addr
        }

data NodeMsg tx =
      BlockMsg   (Block tx)
    | TxMsg      tx
    deriving (Show, Eq, Generic)

instance Binary tx => Binary (NodeMsg tx)

instance (Hashable tx, Binary tx) => Protocol (Nakamoto tx) where
    type Addr (Nakamoto tx) = Word8
    type Msg  (Nakamoto tx) = NodeMsg tx

    step :: Nakamoto tx
         -> Tick
         -> Maybe (Addr (Nakamoto tx), NodeMsg tx)
         -> (Nakamoto tx, [(Addr (Nakamoto tx), NodeMsg tx)])
    step node@Nakamoto{nkPeers, nkChain} _ (Just (_from, msg)) =
        case msg of
            BlockMsg blk | blockPrevHash (blockHeader blk) == blockHash (tip nkChain)
                         , Right _ <- validateBlock blk ->
                (commitBlock blk node, broadcast (BlockMsg blk) nkPeers)
            TxMsg tx | isNovel tx node -> -- TODO: Validate tx.
                (receiveTx tx node, broadcast (TxMsg tx) nkPeers)
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

commitBlock :: Block tx -> Nakamoto tx -> Nakamoto tx
commitBlock blk node =
    node { nkChain = blk |> nkChain node }

mineBlock
    :: Binary tx
    => Tick
    -> Nakamoto tx
    -> (Nakamoto tx, [(Addr (Nakamoto tx), NodeMsg tx)])
mineBlock t node@Nakamoto{nkChain, nkRandom, nkMempool, nkPeers} =
    if   r < 0.1
    then case findBlock t (hash $ blockHeader $ tip $ nkChain) minDifficulty nkMempool of
        Just blk ->
            (commitBlock blk node, broadcast (BlockMsg blk) (toList nkPeers))
        Nothing ->
            (node, [])
    else (node, [])
  where
    r = head nkRandom

-- | Calculate block difficulty.
difficulty :: BlockHeader -> Difficulty
difficulty = os2ip . hash

-- | The minimum difficulty.
minDifficulty :: Difficulty
minDifficulty =
    0x0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

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
