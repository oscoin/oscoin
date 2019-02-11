module Oscoin.Test.Consensus.Nakamoto
    ( NakamotoNodeState
    ) where

import           Oscoin.Prelude

import           Oscoin.Clock
import           Oscoin.Consensus.Mining (mineBlock)
import           Oscoin.Consensus.Nakamoto (PoW(..), emptyPoW)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import           Oscoin.State.Tree (Tree)
import           Oscoin.Storage.Receipt
import           Oscoin.Storage.State.Class (MonadStateStore)

import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Node

import           Codec.Serialise (Serialise)
import qualified Data.Hashable as Hashable
import qualified Data.Map.Strict as Map
import           Lens.Micro
import           System.Random

type NakamotoConsensus tx m = Consensus tx PoW (NakamotoT tx m)

nakConsensus :: (Serialise tx, Monad m) => NakamotoConsensus tx m
nakConsensus = Consensus
    { cScore = comparing Nakamoto.chainScore
    , cMiner = \_chain hdr -> do gen <- state split
                                 pure $ mineBlockRandom gen hdr
    , cValidate = Nakamoto.validateBlock
    }

newtype NakamotoT tx m a =
    NakamotoT (StateT StdGen m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadState StdGen
             , MonadTrans
             )

instance (Monad m, MonadClock m) => MonadClock (NakamotoT tx m)

instance MonadMempool      tx     m => MonadMempool      tx      (NakamotoT tx m)
instance MonadReceiptStore tx ()  m => MonadReceiptStore tx ()   (NakamotoT tx m)

instance MonadStateStore Tree m => MonadStateStore Tree (NakamotoT tx m)

runNakamotoT :: StdGen -> NakamotoT tx m a -> m (a, StdGen)
runNakamotoT rng (NakamotoT ma) = runStateT ma rng

-- | Mine a block header in 10% of the cases. The forged header does
-- not have a valid proof of work.
mineBlockRandom :: StdGen -> BlockHeader s -> Maybe (BlockHeader PoW)
mineBlockRandom stdGen bh@BlockHeader{..} =
    let r = fst $ randomR (0, 1) stdGen
        p = 0.1 :: Float
     in if r < p
           then Just $ bh $> emptyPoW
           else Nothing

type NakamotoNode = NakamotoT DummyTx (TestNodeT PoW Identity)

data NakamotoNodeState = NakamotoNodeState
    { nakStdGen :: StdGen
    , nakNode   :: TestNodeState PoW
    } deriving Show

instance HasTestNodeState PoW NakamotoNodeState where
    testNodeStateL = lens nakNode (\s nakNode -> s { nakNode })

instance TestableNode PoW NakamotoNode NakamotoNodeState where
    testableTick tn =
        withTestBlockStore $ \bs -> mineBlock bs nakConsensus dummyEval tn
    testableInit    = initNakamotoNodes
    testableRun     = runNakamotoNode
    testableScore   = const Nakamoto.chainScore

instance LiftTestNodeT PoW NakamotoNode where
    liftTestNodeT = lift

nakamotoNode :: DummyNodeId -> NakamotoNodeState
nakamotoNode nid = NakamotoNodeState
    { nakStdGen = mkStdGen (Hashable.hash nid)
    , nakNode   = emptyTestNodeState dummyPoW nid
    }
  where
    dummyPoW = sealBlock emptyPoW

initNakamotoNodes :: TestNetwork PoW a -> TestNetwork PoW NakamotoNodeState
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
