{-# LANGUAGE UndecidableInstances #-}
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
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Node.Mempool.Class (MonadMempool(..))
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Receipt
import           Oscoin.Storage.State.Class (MonadStateStore)

import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Node
import           Oscoin.Test.Crypto

import           Codec.Serialise (Serialise)
import qualified Data.Hashable as Hashable
import qualified Data.Map.Strict as Map
import           Lens.Micro
import           System.Random

type NakamotoConsensus c tx m = Consensus c tx PoW (NakamotoT tx m)

nakConsensus :: (IsCrypto c, Serialise tx, Monad m) => NakamotoConsensus c tx m
nakConsensus = Consensus
    { cScore = comparing Nakamoto.chainScore
    , cMiner = \_chain blk -> do gen <- state split
                                 pure $ mineBlockRandom gen blk
    , cValidate = Nakamoto.validateFull
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

instance MonadMempool    c   tx     m => MonadMempool   c    tx      (NakamotoT tx m)
instance MonadReceiptStore c tx ()  m => MonadReceiptStore c tx ()   (NakamotoT tx m)

instance MonadStateStore c st m => MonadStateStore c st (NakamotoT tx m)

runNakamotoT :: StdGen -> NakamotoT tx m a -> m (a, StdGen)
runNakamotoT rng (NakamotoT ma) = runStateT ma rng

-- | Mine a block header in 10% of the cases. The forged header does
-- not have a valid proof of work.
mineBlockRandom
    :: (IsCrypto c)
    => StdGen
    -> Block c tx Unsealed
    -> Maybe (Block c tx (Sealed c PoW))
mineBlockRandom stdGen blk =
    let r = fst $ randomR (0, 1) stdGen
        p = 0.1 :: Float
     in if r < p
           then Just $ sealBlock emptyPoW blk
           else Nothing

type NakamotoNode c = NakamotoT DummyTx (TestNodeT c PoW Identity)

data NakamotoNodeState c = NakamotoNodeState
    { nakStdGen :: StdGen
    , nakNode   :: TestNodeState c PoW
    }

deriving instance Show (Crypto.Hash c) => Show (NakamotoNodeState c)

instance HasTestNodeState c PoW (NakamotoNodeState c) where
    testNodeStateL = lens nakNode (\s nakNode -> s { nakNode })

instance (IsCrypto c) => TestableNode c PoW (NakamotoNode c) (NakamotoNodeState c) where
    testableTick tn =
        withTestBlockStore $ \(publicAPI, privateAPI) -> do
            -- NOTE (adn): We are bypassing the protocol at the moment, but we
            -- probably shouldn't.
            res <- mineBlock publicAPI nakConsensus dummyEval tn
            case res of
              Nothing -> pure Nothing
              Just blk -> do
                  Abstract.insertBlock privateAPI blk
                  pure (Just blk)

    testableInit    = initNakamotoNodes
    testableRun     = runNakamotoNode
    testableScore   = const Nakamoto.chainScore

instance LiftTestNodeT c PoW (NakamotoNode c) where
    liftTestNodeT = lift

nakamotoNode
    :: ( IsCrypto c
       )
    => DummyNodeId
    -> NakamotoNodeState c
nakamotoNode nid = NakamotoNodeState
    { nakStdGen = mkStdGen (Hashable.hash nid)
    , nakNode   = emptyTestNodeState dummyPoW nid
    }
  where
    dummyPoW = sealBlock emptyPoW

initNakamotoNodes
    :: ( IsCrypto c
       )
    => TestNetwork c PoW a
    -> TestNetwork c PoW (NakamotoNodeState c)
initNakamotoNodes tn@TestNetwork{tnNodes} =
    tn { tnNodes = Map.mapWithKey (const . nakamotoNode) tnNodes }

runNakamotoNode :: NakamotoNodeState c -> NakamotoNode c a -> (a, NakamotoNodeState c)
runNakamotoNode s@NakamotoNodeState{..} ma =
    (a, s { nakStdGen = g, nakNode = tns })
  where
    ((a, g), tns) =
        runIdentity
            . runTestNodeT nakNode
            $ runNakamotoT nakStdGen ma
