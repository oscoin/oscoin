module Oscoin.Consensus.Simple
    ( HasSelf(..)
    , HasPeers(..)
    , MonadLastTime(..)

    , simpleConsensus
    , mineSimple
    , reconcileSimple

    , epochLength
    , chainScore
    , shouldCutBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock (Tick)
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore(..))
import           Oscoin.Consensus.Types (Consensus(..), Miner)
import           Oscoin.Crypto.Blockchain (Blockchain, height, tip)
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), BlockHash, BlockHeader(..))

import qualified Data.Set as Set
import           Lens.Micro (Lens')
import           Lens.Micro.Mtl (view)

class HasSelf a i | a -> i where
    self :: Lens' a i

class HasPeers a i | a -> i where
    peers :: Lens' a (Set i)

class Monad m => MonadLastTime m where
    getLastBlockTick :: m Tick
    setLastBlockTick :: Tick -> m ()

    getLastAskTick   :: m Tick
    setLastAskTick   :: Tick -> m ()

epochLength :: Tick
epochLength = 1

simpleConsensus
    :: ( MonadLastTime m
       , HasSelf  r i
       , HasPeers r i
       , Ord        i
       )
    => r
    -> Consensus tx m
simpleConsensus r = Consensus
    { cScore = comparing chainScore
    , cMiner = mineSimple r
    }

mineSimple
    :: ( MonadLastTime m
       , HasSelf  r i
       , HasPeers r i
       , Ord        i
       )
    => r
    -> Miner m
mineSimple r bh@BlockHeader{blockTimestamp} = do
    lastBlk <- getLastBlockTick
    -- FIXME(kim): this seems wrong
    let blockHeaderTick = fromInteger $ toInteger blockTimestamp
    if shouldCutBlock lastBlk r blockHeaderTick
    then do
        setLastBlockTick blockHeaderTick
        pure $ Just bh
    else
        pure Nothing

reconcileSimple
    :: ( MonadBlockStore tx () m
       , MonadLastTime         m
       )
    => Tick
    -> m [BlockHash]
reconcileSimple tick = do
    lastAsk <- getLastAskTick
    if shouldReconcile lastAsk tick
    then do
        setLastAskTick tick
        toList <$> orphans
    else
        pure []

chainScore :: Blockchain tx s -> Int
chainScore bc =
    (bigMagicNumber * h) - steps
  where
    h              = height bc
    lastBlock      = tip bc
    timestampNs    = blockTimestamp $ blockHeader lastBlock
    timestamp      = timestampNs `div` 1000000000000
    e              = round epochLength
    steps          = fromIntegral timestamp `div` e :: Int
    bigMagicNumber = 2526041640 -- some loser in 2050 has to deal with this bug

shouldReconcile :: Tick -> Tick -> Bool
shouldReconcile lastAsk at = time - round lastAsk >= stepTime
  where
    time     = round at :: Int
    stepTime = round epochLength

shouldCutBlock :: (Ord i, HasSelf r i, HasPeers r i) => Tick -> r -> Tick -> Bool
shouldCutBlock lastBlk r at = beenAWhile && ourTurn
  where
    time              = round at
    stepTime          = round epochLength
    nTotalPeers       = 1 + Set.size (view peers r)
    relativeBlockTime = stepTime * nTotalPeers
    beenAWhile        = time - round lastBlk >= relativeBlockTime
    stepNumber        = time `div` stepTime
    ourOffset         = Set.size $ Set.filter (< view self r) (view peers r)
    currentOffset     = stepNumber `mod` nTotalPeers
    ourTurn           = currentOffset == ourOffset
