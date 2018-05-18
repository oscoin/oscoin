module Oscoin.Consensus.Nakamoto where

import           Oscoin.Prelude

import           Oscoin.Consensus.Class
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (hash)
import qualified Oscoin.Node as Node

import           Crypto.Number.Serialize (os2ip)
import           Data.Binary
import qualified Data.List.NonEmpty as NonEmpty
import           Network.Socket (SockAddr)

newtype Nakamoto node = Nakamoto { fromNakamoto :: node }

data NodeMsg tx =
      MsgBlock   (Block tx)
    | MsgTx      tx
    deriving (Show, Eq, Generic)

instance Binary tx => Binary (NodeMsg tx)

instance Protocol (Nakamoto (Node.Handle tx)) where
    type Addr (Nakamoto (Node.Handle tx)) = SockAddr
    type Msg  (Nakamoto (Node.Handle tx)) = NodeMsg tx

    step :: Nakamoto (Node.Handle tx)
         -> Tick
         -> Maybe (SockAddr, NodeMsg tx)
         -> (Nakamoto (Node.Handle tx), [(SockAddr, NodeMsg tx)])
    step node _ (Just (_from, msg)) =
        case msg of
            MsgBlock blk ->
                receiveBlock node blk
            MsgTx tx ->
                receiveTx node tx
    step node _ Nothing =
        mineBlock node

    epoch _ = 1

receiveBlock :: a
receiveBlock = notImplemented

receiveTx :: a
receiveTx = notImplemented

mineBlock :: a
mineBlock = notImplemented

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
