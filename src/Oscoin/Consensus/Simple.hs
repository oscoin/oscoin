module Oscoin.Consensus.Simple where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class

import           Control.Monad.State (State)
import qualified Control.Monad.State as State

import           Data.List (sort)
import           Data.Time.Clock (NominalDiffTime)

data SimpleNode tx = SimpleNode
    { snAddr    :: Addr (SimpleNode tx)
    , snPeers   :: [Addr (SimpleNode tx)]
    , snBuffer  :: [Msg (SimpleNode tx)]
    , snTick    :: Tick (SimpleNode tx)
    , snState   :: [tx]
    } deriving (Eq, Ord, Show)

instance Ord tx => Protocol (SimpleNode tx) where
    type Msg  (SimpleNode tx) = tx
    type Addr (SimpleNode tx) = Word8
    type Tick (SimpleNode tx) = NominalDiffTime

    step sn@SimpleNode{..} _ (Just (_, msg))
        | msg `elem` snState =
            (sn, [])
        | otherwise =
            (sn { snState = state', snBuffer = msg : snBuffer }, [])
      where
        ((), state')  = runSimpleView (apply Nothing [msg]) snState

    step sn@SimpleNode{..} tick Nothing
        | tick - snTick > epoch sn =
            (sn { snTick = tick, snBuffer = [] }, outgoing)
        | otherwise =
            (sn, [])
      where
        outgoing = [(p, msg) | msg <- snBuffer, p <- snPeers]

    epoch _ = 10

newtype SimpleView (f :: * -> *) tx a = SimpleView (State (f tx) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState (f tx)
             )

runSimpleView :: SimpleView f tx a -> f tx -> (a, f tx)
runSimpleView (SimpleView inner) xs =
    runState inner xs

instance Ord tx => View (SimpleView [] tx) where
    type Transaction (SimpleView [] tx) = tx
    type BlockHeader (SimpleView [] tx) = ()

    apply _ txs =
        for_ txs $ \tx ->
            State.modify (\txs -> sort $ tx : txs)
