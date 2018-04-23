module Oscoin.Consensus.Test.Network.Arbitrary where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class

import           Oscoin.Consensus.Test.Network

import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)

import           Test.QuickCheck

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 11

arbitraryNetwork :: TestableNode a => Gen (TestNetwork a)
arbitraryNetwork = arbitrary

arbitraryHealthyNetwork :: forall a. TestableNode a => Gen (TestNetwork a)
arbitraryHealthyNetwork = do
    addrs <- Set.fromList <$> vectorOf kidSize arbitrary :: Gen (Set (Addr a))

    nodes <- forM (toList addrs) $ \a ->
        pure (a, testableNode a [x | x <- toList addrs, x /= a])

    smsgs <- resize kidSize . listOf1 $ do
        msg <- arbitrary  :: Gen (Addr a, Msg a)
        dests <- sublistOf (toList addrs) :: Gen [Addr a]
        forM dests $ \d -> do
            at <- choose (0, 100) :: Gen Int
            pure $ ScheduledMessage (fromIntegral at) d msg

    ticks <- forM nodes $ \(addr, n) ->
        pure [ScheduledTick (epoch n * fromIntegral x) addr | x <- [0..100] :: [Int]]

    pure TestNetwork
        { tnNodes      = Map.fromList nodes
        , tnMsgs       = Set.fromList (concat (smsgs ++ ticks))
        , tnPartitions = Map.empty
        }

arbitraryPartitionedNetwork :: TestableNode a => Tick a -> Gen (TestNetwork a)
arbitraryPartitionedNetwork at = do
    net@TestNetwork{..} <- arbitraryHealthyNetwork
    part                <- oneof [ arbitraryPerfectPartition (Map.keys tnNodes)
                                 , arbitraryLonerPartition   (Map.keys tnNodes) ]
    pure $ net { tnMsgs = Set.insert (Partition at part) tnMsgs }

arbitraryDisconnects :: TestableNode a => [Addr a] -> Gen [Scheduled a]
arbitraryDisconnects addrs =
    vectorOf kidSize $ do
        at <- choose (0, 100) :: Gen Int
        from <- elements addrs
        to <- elements addrs
        pure $ Disconnect (fromIntegral at) from to

arbitraryPerfectPartition :: Ord addr => [addr] -> Gen (Map addr (Set addr))
arbitraryPerfectPartition [] =
    pure mempty
arbitraryPerfectPartition addrs = do
    (l, r) <- splitAt middle <$> shuffle addrs
    pure $ Map.fromList $ [(addr, Set.fromList (filter (/= addr) l)) | addr <- l]
                       ++ [(addr, Set.fromList (filter (/= addr) r)) | addr <- r]
  where
    middle = length addrs `div` 2

arbitraryLonerPartition :: Ord addr => [addr] -> Gen (Map addr (Set addr))
arbitraryLonerPartition addrs = do
    addrs' <- shuffle addrs
    pure . Map.fromList $ case addrs' of
        a' : as ->
            [(a', mempty)] ++ [(a, Set.fromList (filter (/= a) as)) | a <- as]
        [] ->
            mempty

instance TestableNode a => Arbitrary (TestNetwork a) where
    arbitrary = do
        network@TestNetwork{..} <- arbitraryHealthyNetwork
        disconnects             <- arbitraryDisconnects (Map.keys tnNodes)
        pure $ network { tnMsgs = tnMsgs ++ Set.fromList disconnects }

    shrink (TestNetwork nodes msgs partitions) =
        map filterNetwork lessNodes ++ lessMsgs
      where
        msgs'     = shrinkScheduledMsgs msgs
        nodes'    = shrinkList shrinkNothing (Map.toList nodes)
        lessMsgs  = [TestNetwork nodes ms partitions                | ms <- msgs' ]
        lessNodes = [TestNetwork (Map.fromList ns) msgs partitions  | ns <- nodes']

shrinkScheduledMsgs :: Ord (Scheduled a) => Set (Scheduled a) -> [Set (Scheduled a)]
shrinkScheduledMsgs msgs =
    [Set.filter (not . isMsg) msgs <> msgs | msgs <- shrinkedMsgs]
  where
    shrinkedMsgs =
        shrinkMapBy
            Set.fromList
            Set.toList
            (shrinkList shrinkNothing)
            (Set.filter isMsg msgs)

filterNetwork :: Ord (Addr a) => TestNetwork a -> TestNetwork a
filterNetwork (TestNetwork nodes msgs partitions) =
    TestNetwork nodes (Set.filter f msgs) partitions
  where
    f msg = all (`Map.member` nodes) $
        scheduledReceivers msg ++ catMaybes [scheduledSender msg]

