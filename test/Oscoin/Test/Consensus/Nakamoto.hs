module Oscoin.Test.Consensus.Nakamoto
    ( NakamotoNodeState
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import           Oscoin.Consensus.Class (MonadUpdate)
import           Oscoin.Consensus.Evaluator (identityEval)
import           Oscoin.Consensus.Mining (mineBlock)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import           Oscoin.Node.Mempool.Class (MonadMempool(..))

import           Oscoin.Test.Consensus.Class
import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Node

import qualified Data.Hashable as Hashable
import qualified Data.Map.Strict as Map
import           Lens.Micro
import           System.Random


type NakamotoConsensus tx s m = Consensus tx (NakamotoT tx s m)

nakConsensus :: Monad m => NakamotoConsensus tx s m
nakConsensus = Consensus
    { cScore = comparing Nakamoto.chainScore
    , cMiner = \_chain hdr -> do gen <- state split
                                 pure $ mineBlockRandom gen hdr
    }

newtype NakamotoT tx s m a =
    NakamotoT (StateT StdGen m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState StdGen
             , MonadTrans
             )

instance (Monad m, MonadClock m) => MonadClock (NakamotoT tx s m)

instance MonadMempool     tx   m => MonadMempool     tx   (NakamotoT tx s m)
instance MonadBlockStore  tx s m => MonadBlockStore  tx s (NakamotoT tx s m)
instance MonadUpdate         s m => MonadUpdate         s (NakamotoT tx s m)

runNakamotoT :: StdGen -> NakamotoT tx s m a -> m (a, StdGen)
runNakamotoT rng (NakamotoT ma) = runStateT ma rng

-- | Mine a block header in 10% of the cases. The forged header does
-- not have a valid proof of work.
mineBlockRandom :: StdGen -> BlockHeader a -> Maybe (BlockHeader a)
mineBlockRandom stdGen bh@BlockHeader{..} =
    let r = fst $ randomR (0, 1) stdGen
        p = 0.1 :: Float
     in if r < p
           then Just bh
           else Nothing

type NakamotoNode = NakamotoT DummyTx () (TestNodeT Identity)

data NakamotoNodeState = NakamotoNodeState
    { nakStdGen :: StdGen
    , nakNode   :: TestNodeState
    } deriving Show

instance HasTestNodeState NakamotoNodeState where
    testNodeStateL = lens nakNode (\s nakNode -> s { nakNode })

instance TestableNode NakamotoNode NakamotoNodeState where
    testableTick tick = do
        let time = timeAdd epoch tick
        blk <- (map . map) void $ mineBlock nakConsensus identityEval time
        pure $ maybeToList (BlockMsg <$> blk)
    testableInit = initNakamotoNodes
    testableRun  = runNakamotoNode
    testableScore = const Nakamoto.chainScore

nakamotoNode :: DummyNodeId -> NakamotoNodeState
nakamotoNode nid = NakamotoNodeState
    { nakStdGen = mkStdGen (Hashable.hash nid)
    , nakNode   = emptyTestNodeState nid
    }

initNakamotoNodes :: TestNetwork a -> TestNetwork NakamotoNodeState
initNakamotoNodes tn@TestNetwork{tnNodes} =
    tn { tnNodes = Map.mapWithKey (const . nakamotoNode) tnNodes }

runNakamotoNode :: NakamotoNodeState -> NakamotoNode a -> (a, NakamotoNodeState)
runNakamotoNode s@NakamotoNodeState{..} ma =
    (a, s { nakStdGen = g, nakNode = tns })
  where
    ((a, g), tns) =
        runIdentity
            . runTestNodeT nakNode
            $ runNakamotoT nakStdGen ma
