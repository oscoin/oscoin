module Oscoin.Test.Consensus.Nakamoto
    ( NakamotoT
    , runNakamotoT

    , Nakamoto.epochLength
    , Nakamoto.difficulty
    , Nakamoto.minDifficulty
    , Nakamoto.easyDifficulty
    , Nakamoto.defaultGenesisDifficulty
    , Nakamoto.chainDifficulty
    , Nakamoto.hasPoW
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import           Oscoin.Consensus.Class (MonadUpdate)
import           Oscoin.Consensus.Evaluator (identityEval)
import           Oscoin.Consensus.Mining (mineBlock)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import           Oscoin.Node.Mempool.Class (MonadMempool(..))

import           Oscoin.Test.Consensus.Class

import           Codec.Serialise (Serialise)
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

instance ( MonadMempool    tx   m
         , MonadBlockStore tx s m
         , Serialise       tx
         ) => MonadProtocol tx s (NakamotoT tx s m)
  where
    mineM t = (map . map) void $ mineBlock nakConsensus identityEval t

    reconcileM = const $ pure mempty

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
