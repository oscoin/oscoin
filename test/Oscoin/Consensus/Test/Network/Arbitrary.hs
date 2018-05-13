module Oscoin.Consensus.Test.Network.Arbitrary where

import           Oscoin.Prelude
import           Oscoin.Consensus.Class

import           Oscoin.Consensus.Test.Network

import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.List (nub)

import           Test.QuickCheck

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 11

-- | Converts a Tick to seconds.
toSeconds :: Tick -> Int
toSeconds t = fromEnum t `div` 1000000000000

arbitraryNetwork :: TestableNode a => Gen (TestNetwork a)
arbitraryNetwork = arbitrary

arbitraryHealthyNetwork :: forall a. TestableNode a => Gen (TestNetwork a)
arbitraryHealthyNetwork = do
    addrs <- Set.fromList <$> resize kidSize (listOf arbitrary)
        `suchThat` (\as -> nub as == as && odd (length as)) :: Gen (Set (Addr a))

    nodes <- forM (toList addrs) $ \a ->
        pure (a, testableNode a [x | x <- toList addrs, x /= a])

    e <- pure . epoch . snd . head $ nodes

    let lastTick = (length addrs * 3) * toSeconds e :: Int

    smsgs <- listOf1 $ do
        msg <- arbitrary  :: Gen (Addr a, Msg a)
        dests <- sublistOf (toList addrs) :: Gen [Addr a]
        forM dests $ \d -> do
            at <- choose (0, lastTick `div` 2) :: Gen Int
            pure $ ScheduledMessage (fromIntegral at) d msg

    let ticks = foreach nodes $ \(addr, _) ->
         [ScheduledTick (fromIntegral sec) addr | sec <- [0..lastTick]]

    pure TestNetwork
        { tnNodes      = Map.fromList nodes
        , tnMsgs       = Set.fromList (concat (smsgs ++ ticks))
        , tnPartitions = Map.empty
        , tnLog        = []
        }

arbitraryPartitionedNetwork :: TestableNode a => Gen (TestNetwork a)
arbitraryPartitionedNetwork = do
    net@TestNetwork{..} <- arbitraryHealthyNetwork
    partition           <- arbitraryPartition (Map.keys tnNodes)
    partitionAt         <- toEnum <$> choose ( fromEnum $ scheduledTick (minimum tnMsgs)
                                             , fromEnum $ scheduledTick (maximum tnMsgs) / 2) :: Gen Tick
    healAt              <- toEnum <$> choose ( fromEnum $ partitionAt
                                             , fromEnum $ scheduledTick (maximum tnMsgs) / 1.5) :: Gen Tick
    pure $ net { tnMsgs = tnMsgs ++ Set.fromList [Partition partitionAt partition, Heal healAt] }

arbitraryDisconnects :: [Addr a] -> Gen [Scheduled a]
arbitraryDisconnects addrs =
    vectorOf kidSize $ do
        at <- choose (0, 100) :: Gen Int
        from <- elements addrs
        to <- elements addrs
        pure $ Disconnect (fromIntegral at) from to

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

instance TestableNode a => Arbitrary (TestNetwork a) where
    arbitrary = arbitraryPartitionedNetwork

    shrink (TestNetwork nodes msgs partitions _) =
        lessMsgs
      where
        msgs'     = shrinkScheduledMsgs msgs
        nodes'    = shrinkList shrinkNothing (Map.toList nodes)
        lessMsgs  = [TestNetwork nodes ms partitions [] | ms <- msgs']

        -- NB. Not in use currently.
        _lessNodes = map filterNetwork [TestNetwork (Map.fromList ns) msgs partitions [] | ns <- nodes']

shrinkScheduledMsgs :: Ord (Scheduled a) => Set (Scheduled a) -> [Set (Scheduled a)]
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
filterNetwork :: Ord (Addr a) => TestNetwork a -> TestNetwork a
filterNetwork (TestNetwork nodes msgs partitions _) =
    TestNetwork nodes (Set.filter f msgs) partitions []
  where
    f msg = all (`Map.member` nodes) $
        scheduledReceivers msg ++ catMaybes [scheduledSender msg]

