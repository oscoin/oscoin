module Oscoin.Consensus.Test.Node
    ( TestNode(..)
    , BufferedTestNode(..)
    ) where

import Oscoin.Prelude
import Oscoin.Consensus.Test.View
import Oscoin.Consensus.Class

import Data.Time.Clock (NominalDiffTime)

data TestNode tx = TestNode (Addr (TestNode tx)) [tx] [Addr (TestNode tx)]
    deriving (Eq, Ord, Show)

instance (Ord tx, Eq tx) => Protocol (TestNode tx) where
    type Msg  (TestNode tx) = tx
    type Addr (TestNode tx) = Word8
    type Tick (TestNode tx) = NominalDiffTime

    step tn@(TestNode a state peers) _at (Just (_, msg))
        | msg `elem` state = (tn, [])
        | otherwise        = (TestNode a state' peers, broadcastMsgs)
      where
        ((), state')  = runDummyView (apply Nothing [msg]) state
        broadcastMsgs = map (\p -> (p, msg)) peers
    step tn _at Nothing =
        (tn, [])

    epoch _ = 1

-------------------------------------------------------------------------------

data BufferedTestNode tx = BufferedTestNode
    { btnAddr    :: Addr (BufferedTestNode tx)
    , btnPeers   :: [Addr (BufferedTestNode tx)]
    , btnBuffer  :: [Msg (BufferedTestNode tx)]
    , btnTick    :: Tick (BufferedTestNode tx)
    , btnState   :: [tx]
    } deriving (Eq, Ord, Show)

instance Ord tx => Protocol (BufferedTestNode tx) where
    type Msg  (BufferedTestNode tx) = tx
    type Addr (BufferedTestNode tx) = Word8
    type Tick (BufferedTestNode tx) = NominalDiffTime

    step btn@BufferedTestNode{..} _ (Just (_, msg))
        | msg `elem` btnState =
            (btn, [])
        | otherwise =
            (btn { btnState = state', btnBuffer = msg : btnBuffer }, [])
      where
        ((), state')  = runDummyView (apply Nothing [msg]) btnState

    step btn@BufferedTestNode{..} tick Nothing
        | tick - btnTick > epoch btn =
            (btn { btnTick = tick, btnBuffer = [] }, outgoing)
        | otherwise =
            (btn, [])
      where
        outgoing = [(p, msg) | msg <- btnBuffer, p <- btnPeers]

    epoch _ = 10

