{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Consensus.Network.Arbitrary where

import           Oscoin.Prelude


import           Oscoin.Test.Consensus.Class (Msg(..))
import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Node (DummyNodeId)

import           Data.List (nub, permutations)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Traversable (for)

import           System.Random
import           Test.QuickCheck

-- | Smaller tests for computationally complex generators.
kidSize :: Int
kidSize = 13

arbitraryTxMsg :: Arbitrary tx => Gen (Msg tx)
arbitraryTxMsg = TxMsg <$> arbitrary

arbitraryNetwork :: Gen (TestNetwork ())
arbitraryNetwork = arbitrary

arbitraryHealthyNetwork :: Tick -> Gen (TestNetwork ())
arbitraryHealthyNetwork e = do
    net  <- arbitrarySynchronousNetwork e
    gen  <- mkStdGen <$> arbitrary :: Gen StdGen

    pure net
        { tnLatencies  = map fromIntegral (randomRs (1 :: Int, 1 + 2 * toSeconds e) gen) }

arbitrarySynchronousNetwork :: Tick -> Gen (TestNetwork ())
arbitrarySynchronousNetwork e = do
    addrs <- Set.fromList <$> resize kidSize (listOf arbitrary)
        `suchThat` (\as -> nub as == as && odd (length as)) :: Gen (Set DummyNodeId)

    let nodes    = zip (toList addrs) (repeat ())
    let lastTick = max ((length addrs * 3) * toSeconds e) (toSeconds 30) :: Int

    smsgs <- listOf1 $ do
        (sender, msg) <- liftA2 (,) arbitrary arbitraryTxMsg
        dests <- sublistOf (toList addrs) :: Gen [DummyNodeId]
        for dests $ \d -> do
            at <- choose (0, lastTick `div` 2) :: Gen Int
            pure $ ScheduledMessage (fromIntegral at) d sender msg

    let ticks = foreach nodes $ \(addr, _) ->
         [ScheduledTick (fromIntegral sec) addr | sec <- [0..lastTick]]

    rng <- mkStdGen <$> arbitrary

    pure TestNetwork
        { tnNodes      = Map.fromList nodes
        , tnMsgs       = Set.fromList (concat (smsgs ++ ticks))
        , tnPartitions = Map.empty
        , tnLog        = []
        , tnLatencies  = repeat 0
        , tnRng        = rng
        , tnMsgCount   = 0
        , tnLastTick   = fromIntegral lastTick
        }

type Tick = Int64

arbitraryPartitionedNetwork :: Tick -> Gen (TestNetwork ())
arbitraryPartitionedNetwork e = do
    net@TestNetwork{..} <- arbitraryHealthyNetwork e
    partition           <- arbitraryPartition (Map.keys tnNodes)
    partitionAt         <- choose ( scheduledTick (minimum tnMsgs)
                                  , scheduledTick (maximum tnMsgs) `div` 4 )
    healAt              <- choose ( partitionAt
                                  , scheduledTick (maximum tnMsgs) `div` 3 )

    let partheal = Set.fromList [Partition partitionAt partition, Heal healAt]
    pure net { tnMsgs = tnMsgs <> partheal }

arbitraryPartition :: Ord addr => [addr] -> Gen (Map addr (Set addr))
arbitraryPartition addrs =
    oneof [ arbitraryPerfectPartition addrs
          , arbitraryLonerPartition   addrs
          , arbitraryBridgePartition  addrs
          , arbitraryDoublePartition  addrs
          ]

arbitraryDoublePartition :: Ord addr => [addr] -> Gen (Map addr (Set addr))
arbitraryDoublePartition addrs
    | length addrs < 3 =
        pure mempty
    | otherwise =
        subnetsToPartitions . chunksOf subnetSize <$> shuffle addrs
      where
        netSize = length addrs
        subnetSize | odd netSize = netSize `div` 2
                   | otherwise   = netSize `div` 2 - 1

-- | Convert a list of subnets into a list partition map.
subnetsToPartitions :: Ord addr => [[addr]] -> Map addr (Set addr)
subnetsToPartitions nets =
    Map.fromList $
        concatMap f [(p, concat ps) | p:ps <- perms]
  where
    f (xs, ys) = [(x, Set.fromList ys) | x <- xs]
    perms      = take (length nets) (permutations nets)

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

    shrink tn =
        lessMsgs ++ lessNodes
      where
        msgs'     = shrinkScheduledMsgs (tnMsgs tn)
        nodes'    = shrinkList shrinkNothing (Map.toList $ tnNodes tn)
        lessMsgs  = [tn { tnMsgs = ms } | ms <- msgs']
        lessNodes = filter (odd . length . tnNodes)
                  $ filter (not . null . tnNodes)
                  $ [filterNetwork tn { tnNodes = Map.fromList ns } | ns <- nodes']

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

filterNetwork :: TestNetwork a -> TestNetwork a
filterNetwork tn@TestNetwork{..} =
    tn { tnMsgs = Set.filter f tnMsgs }
  where
    f msg = all (`Map.member` tnNodes) (scheduledReceivers msg)

--------------------------------------------------------------------------------

toSeconds :: Tick -> Int
toSeconds = round @Float . realToFrac

-- | Splits a list into length-@n@ pieces. If @n@ is @<= 0@, returns an infinite
-- list of empty lists.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0     = take n l : chunksOf n (drop n l)
  | otherwise = repeat []
