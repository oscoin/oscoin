module Oscoin.Consensus.Test.Network.Arbitrary where

import           Oscoin.Prelude

import           Oscoin.Consensus.Class
import           Oscoin.P2P (Msg(..))

import           Oscoin.Consensus.Test.Network
import           Oscoin.Consensus.Test.Node (DummyNodeId)

import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import           Data.Traversable (for)

import           System.Random
import           Test.QuickCheck

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 13

arbitraryTxMsg :: Arbitrary tx => Gen (Msg tx)
arbitraryTxMsg = TxMsg . (:[]) <$> arbitrary

arbitraryNetwork :: Gen (TestNetwork ())
arbitraryNetwork = arbitrary

arbitraryHealthyNetwork :: Tick -> Gen (TestNetwork ())
arbitraryHealthyNetwork e = do
    addrs <- Set.fromList <$> resize kidSize (listOf arbitrary)
        `suchThat` (\as -> nub as == as && odd (length as)) :: Gen (Set DummyNodeId)

    let nodes    = zip (toList addrs) (repeat ())
    let lastTick = (length addrs * 3) * toSeconds e :: Int

    smsgs <- listOf1 $ do
        msg   <- liftA2 (,) arbitrary arbitraryTxMsg
        dests <- sublistOf (toList addrs) :: Gen [DummyNodeId]
        for dests $ \d -> do
            at <- choose (0, lastTick `div` 2) :: Gen Int
            pure $ ScheduledMessage (fromIntegral at) d msg

    let ticks = foreach nodes $ \(addr, _) ->
         [ScheduledTick (fromIntegral sec) addr | sec <- [0..lastTick]]

    seed <- arbitrary :: Gen Int

    pure TestNetwork
        { tnNodes      = Map.fromList nodes
        , tnMsgs       = Set.fromList (concat (smsgs ++ ticks))
        , tnPartitions = Map.empty
        , tnLog        = []
        , tnLatencies  = map fromIntegral (randomRs (1 :: Int, 1 + 2 * toSeconds e) (mkStdGen seed))
        , tnMsgCount   = 0
        , tnLastTick   = fromIntegral lastTick
        }

arbitraryPartitionedNetwork :: Tick -> Gen (TestNetwork ())
arbitraryPartitionedNetwork e = do
    net@TestNetwork{..} <- arbitraryHealthyNetwork e
    partition           <- arbitraryPartition (Map.keys tnNodes)
    partitionAt         <- toEnum <$> choose ( fromEnum $ scheduledTick (minimum tnMsgs)
                                             , fromEnum $ scheduledTick (maximum tnMsgs) / 4) :: Gen Tick
    healAt              <- toEnum <$> choose ( fromEnum $ partitionAt
                                             , fromEnum $ scheduledTick (maximum tnMsgs) / 3) :: Gen Tick

    let partheal = Set.fromList [Partition partitionAt partition, Heal healAt]
    pure net { tnMsgs = tnMsgs <> partheal }

arbitraryPartition :: Ord addr => [addr] -> Gen (Map addr (Set addr))
arbitraryPartition addrs =
    oneof [ arbitraryPerfectPartition addrs
          , arbitraryLonerPartition   addrs
          , arbitraryBridgePartition  addrs
          ]

arbitraryPerfectPartition :: Ord addr => [addr] -> Gen (Map addr (Set addr))
arbitraryPerfectPartition [] =
    pure mempty
arbitraryPerfectPartition addrs = do
    (l, r) <- splitAt middle <$> shuffle addrs
    pure $ Map.fromList $ [(addr, Set.fromList r) | addr <- l]
                       ++ [(addr, Set.fromList l) | addr <- r]
  where
    middle = length addrs `div` 2

arbitraryLonerPartition :: Ord addr => [addr] -> Gen (Map addr (Set addr))
arbitraryLonerPartition addrs = do
    addrs' <- shuffle addrs
    pure . Map.fromList $ case addrs' of
        a' : as ->
            (a', Set.fromList as) : [(a, Set.singleton a') | a <- as]
        [] ->
            mempty

arbitraryBridgePartition :: Ord addr => [addr] -> Gen (Map addr (Set addr))
arbitraryBridgePartition addrs = do
    addrs' <- shuffle addrs
    pure . Map.fromList $ case addrs' of
        _ : as | middle <- length as `div` 2
               , (l, r) <- splitAt middle as ->
            concat [ [(a, Set.fromList r) | a <- l]
                   , [(a, Set.fromList l) | a <- r]
                   ]
        [] ->
            mempty

instance Arbitrary (TestNetwork ()) where
    arbitrary = arbitraryPartitionedNetwork 1 -- TODO: use size for epochLength?

    shrink tn@TestNetwork{..} =
        lessMsgs
      where
        msgs'     = shrinkScheduledMsgs tnMsgs
        nodes'    = shrinkList shrinkNothing (Map.toList tnNodes)
        lessMsgs  = [tn { tnMsgs = ms } | ms <- msgs']

        -- NB. Not in use currently.
        _lessNodes = map filterNetwork [tn { tnNodes = Map.fromList ns } | ns <- nodes']

shrinkScheduledMsgs :: Set Scheduled -> [Set Scheduled]
shrinkScheduledMsgs msgs =
    mempty : [Set.filter (not . isMsg) msgs <> ms | ms <- shrinkedMsgs]
  where
    shrinkedMsgs =
        shrinkMapBy
            Set.fromList
            Set.toList
            (shrinkList shrinkNothing)
            (Set.filter isMsg msgs)

-- TODO: Nodes will still contain previously present nodes in their address
-- books.
filterNetwork :: TestNetwork a -> TestNetwork a
filterNetwork (TestNetwork nodes msgs partitions _ rng count lt) =
    TestNetwork nodes (Set.filter f msgs) partitions [] rng count lt
  where
    f msg = all (`Map.member` nodes) $
        scheduledReceivers msg ++ catMaybes [scheduledSender msg]
