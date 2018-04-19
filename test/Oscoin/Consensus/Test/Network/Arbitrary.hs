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

    ticks <- forM nodes $ \(addr, n) -> do
        pure [ScheduledTick (epoch n * fromIntegral x) addr | x <- [0..100] :: [Int]]

    pure $ TestNetwork
        { tnNodes      = Map.fromList nodes
        , tnMsgs       = Set.fromList (concat (smsgs ++ ticks))
        , tnPartitions = Map.empty
        }

arbitraryDisconnects :: TestableNode a => [Addr a] -> Gen [Scheduled a]
arbitraryDisconnects addrs =
    vectorOf kidSize $ do
        at <- choose (0, 100) :: Gen Int
        from <- elements addrs
        to <- elements addrs
        pure $ Disconnect (fromIntegral at) from to

instance TestableNode a => Arbitrary (TestNetwork a) where
    arbitrary = do
        network@TestNetwork{..} <- arbitraryHealthyNetwork
        disconnects             <- arbitraryDisconnects (Map.keys tnNodes)
        pure $ network { tnMsgs = tnMsgs ++ Set.fromList disconnects }

    shrink (TestNetwork nodes msgs partitions) =
        lessNodes ++ map filterNetwork lessMsgs
      where
        msgs'     = shrinkList shrinkNothing (toList msgs)
        nodes'    = shrinkList shrinkNothing (Map.toList nodes)
        lessMsgs  = [TestNetwork nodes (Set.fromList ms) partitions | ms <- msgs']
        lessNodes = [TestNetwork (Map.fromList ns) msgs partitions  | ns <- nodes']

filterNetwork :: Ord (Addr a) => TestNetwork a -> TestNetwork a
filterNetwork (TestNetwork nodes msgs partitions) =
    TestNetwork nodes (Set.filter f msgs) partitions
  where
    f msg = all (flip Map.member nodes) $
        scheduledReceivers msg ++ catMaybes [scheduledSender msg]

