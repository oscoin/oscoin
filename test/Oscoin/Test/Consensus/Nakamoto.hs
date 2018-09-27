module Oscoin.Test.Consensus.Nakamoto
    ( NakamotoT
    , NakamotoEnv(..)
    , defaultNakamotoEnv
    , runNakamotoT
    , evalNakamotoT

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
import           Oscoin.Consensus.Evaluator (Evaluator, identityEval)
import           Oscoin.Consensus.Mining (mineBlock)
import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Logging as Log
import           Oscoin.Node.Mempool.Class (MonadMempool(..))

import           Oscoin.Test.Consensus.Class

import           Codec.Serialise (Serialise)
import           Control.Monad.RWS (RWST, evalRWST, runRWST, state)
import           Control.Monad.Writer.CPS (MonadWriter)
import           Data.Has (Has(..))
import           System.Random (StdGen, split)

type NakamotoEval tx s = Evaluator s tx ()

-- | A Nakamoto mining function. Tries all nonces and returns 'Nothing' if
-- no block satisfying the difficulty was found.
type NakamotoMiner = forall tx s a.
       StdGen
    -> Blockchain tx s
    -> BlockHeader a
    -> Maybe (BlockHeader a)

-- | Read-only environment for the Nakamoto consensus protocol.
data NakamotoEnv tx s = NakamotoEnv
    { nakEval       :: NakamotoEval tx s
    -- ^ Evaluation function to use for expressions
    , nakDifficulty :: Difficulty
    -- ^ Target difficulty (Nb. this is fixed for now and does not adjust)
    , nakMiner      :: NakamotoMiner
    -- ^ Mining function to use
    , nakLogger     :: Log.Logger
    }

instance Has Log.Logger (NakamotoEnv tx s) where
    getter = nakLogger
    modifier f env = env {nakLogger = f (nakLogger env)}

defaultNakamotoEnv :: NakamotoEnv tx s
defaultNakamotoEnv = NakamotoEnv{..}
  where
    nakEval       = identityEval
    nakDifficulty = Nakamoto.easyDifficulty
    nakLogger     = Log.noLogger

    nakMiner _gen chain hdr =
        join $ Nakamoto.mineNakamoto (const nakDifficulty) chain hdr


nakEnvToConsensus :: Monad m => NakamotoEnv tx s -> Consensus tx (NakamotoT tx s m)
nakEnvToConsensus NakamotoEnv{..} = Consensus
    { cScore = comparing Nakamoto.chainScore
    , cMiner = \chain hdr -> (\gen -> nakMiner gen chain hdr) <$> state split
    }

newtype NakamotoT tx s m a =
    NakamotoT (RWST (NakamotoEnv tx s) () StdGen m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader (NakamotoEnv tx s)
             , MonadWriter ()
             , MonadState StdGen
             , MonadTrans
             )

instance (Monad m, MonadClock m) => MonadClock (NakamotoT tx s m)

instance ( MonadMempool    tx   m
         , MonadBlockStore tx s m
         , Serialise       tx
         ) => MonadProtocol tx s (NakamotoT tx s m)
  where
    mineM t = do
        cons <- nakEnvToConsensus <$> ask
        eval <- asks nakEval
        (map . map) void $ mineBlock cons eval t

    reconcileM = const $ pure mempty

instance MonadMempool     tx   m => MonadMempool     tx   (NakamotoT tx s m)
instance MonadBlockStore  tx s m => MonadBlockStore  tx s (NakamotoT tx s m)
instance MonadUpdate         s m => MonadUpdate         s (NakamotoT tx s m)

runNakamotoT :: NakamotoEnv tx s -> StdGen -> NakamotoT tx s m a -> m (a, StdGen, ())
runNakamotoT env rng (NakamotoT ma) = runRWST ma env rng

evalNakamotoT :: Monad m => NakamotoEnv tx s -> StdGen -> NakamotoT tx s m a -> m (a, ())
evalNakamotoT env rng (NakamotoT ma) = evalRWST ma env rng
