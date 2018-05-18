module Oscoin.Consensus.Nakamoto where

import           Oscoin.Prelude

import           Oscoin.Consensus.Class
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (hash, Hashed)
import           Oscoin.Node.Mempool (Mempool)

import           Crypto.Number.Serialize (os2ip)
import qualified Data.List.NonEmpty as NonEmpty
import           Network.Socket (SockAddr)

data Node tx = Node
    { nodeChains  :: [Blockchain tx]
    , nodeMempool :: Mempool (Hashed tx) tx
    }

data NodeMsg tx =
      MsgPropose (Block tx)
    | MsgBlock   (Block tx)

instance Protocol (Node tx) where
    type Addr (Node tx) = SockAddr
    type Msg  (Node tx) = NodeMsg tx

    step :: Node tx
         -> Tick
         -> Maybe (SockAddr, NodeMsg tx)
         -> (Node tx, [(SockAddr, NodeMsg tx)])
    step node _ (Just (_from, _msg)) =
        (node, [])
    step node _ Nothing =
        (node, [])

    epoch _ = 1

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
isDifficultyValid :: BlockHeader -> Bool
isDifficultyValid header =
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
