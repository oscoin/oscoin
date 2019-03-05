{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Test.Consensus.Network.Arbitrary
    ( arbitraryPartitionedNetwork
    , arbitraryHealthyNetwork
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Eval (identityEval)

import           Oscoin.Test.Consensus.Network
import           Oscoin.Test.Consensus.Node (DummyNodeId)
import           Oscoin.Test.Crypto
import           Oscoin.Time

import           Data.List (nub, permutations)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Traversable (for)

import           System.Random
import           Test.QuickCheck

-- | Maximum number of nodes.
maxNodes :: Int
maxNodes = 13

-- | Maximum number of scheduled messages.
maxScheduled :: Int
maxScheduled = 16

-- | Minimum message latency.
minLatency :: Duration -> Duration
minLatency blockTime = blockTime `div` 3

-- | Maximum message latency.
maxLatency :: Duration -> Duration
maxLatency blockTime = 3 * blockTime

-- | Simulation duration.
simDuration :: Duration -> Duration
simDuration blockTime = 36 * blockTime

arbitraryTxMsg :: Gen (Msg c s)
arbitraryTxMsg = TxMsg <$> arbitrary

-- | A 'TestNetwork' with some latency, and all messages are delivered.
arbitraryHealthyNetwork
    :: ( IsCrypto c
       , Ord s
       )
    => Duration
    -> Gen (TestNetwork c s ())
arbitraryHealthyNetwork blockTime = do
    net  <- arbitrarySynchronousNetwork blockTime
    gen  <- mkStdGen <$> arbitrary :: Gen StdGen

    pure net
        { tnLatencies = randomRs ( minLatency blockTime
                                 , maxLatency blockTime
                                 ) gen }

-- | A 'TestNetwork' with no message latency.
arbitrarySynchronousNetwork
    :: ( IsCrypto c
       , Ord s
       )
     => Duration
     -> Gen (TestNetwork c s ())
arbitrarySynchronousNetwork blockTime = do
    addrs <- resize maxNodes (listOf1 arbitrary)
        `suchThat` (\as -> nub as == as) :: Gen [DummyNodeId]

    let duration = simDuration blockTime

    smsgs <- resize maxScheduled <$> listOf1 $ do
        (sender, msg) <- liftA2 (,) arbitrary arbitraryTxMsg
        dests <- sublistOf addrs :: Gen [DummyNodeId]
        for dests $ \d -> do
            at <- choose (epoch, fromEpoch (duration `div` 2)) :: Gen Timestamp
            pure $ ScheduledMessage at d sender msg

    let ticks = foreach addrs $ \addr ->
         [ScheduledTick (fromEpoch sec) addr | sec <- [0, blockTime .. duration]]

    rng <- mkStdGen <$> arbitrary

    let scheduled = Set.fromList $ concat (smsgs ++ ticks)

    pure TestNetwork
        { tnNodes      = Map.fromList $ zip addrs (repeat ())
        , tnScheduled  = scheduled
        , tnMsgs       = mempty
        , tnPartitions = Map.empty
        , tnLog        = []
        , tnLatencies  = repeat 0
        , tnRng        = rng
        , tnEval       = identityEval
        , tnValidate   = \_ _ -> Right ()
        , tnMsgCount   = 0
        , tnLastTick   = fromEpoch duration
        }

-- | A 'TestNetwork' with some latency, where some messages are dropped.
arbitraryPartitionedNetwork
    :: ( Ord s
       , IsCrypto c
       )
    => Duration
    -> Gen (TestNetwork c s ())
arbitraryPartitionedNetwork blockTime = do
    net@TestNetwork{..} <- arbitraryHealthyNetwork blockTime
    partition           <- arbitraryPartition (Map.keys tnNodes)

    let firstTick = scheduledTick $ minimum tnScheduled
    let lastTick  = scheduledTick $ maximum tnScheduled

    partitionAt <- choose (firstTick, fromEpoch $ sinceEpoch lastTick `div` 4 )
    healAt <- choose (partitionAt, fromEpoch $ sinceEpoch lastTick `div` 3)

    let msgs = Set.fromList [Partition partitionAt partition, Heal healAt]
    pure net { tnScheduled = tnScheduled <> msgs }

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

instance (IsCrypto c, Ord s) => Arbitrary (TestNetwork c s ()) where
    arbitrary = arbitraryPartitionedNetwork 1 -- TODO: use size for epochLength?

    shrink tn =
        lessNodes ++ lessMsgs
      where
        msgs'     = shrinkScheduledMsgs (tnScheduled tn)
        nodes'    = reverse . tailDef [] . tails . Map.toList $ tnNodes tn
        lessMsgs  = [tn { tnScheduled = ms } | ms <- msgs']
        lessNodes = filter (not . null . tnNodes)
                  $ catMaybes [filterNetwork tn { tnNodes = Map.fromList ns } | ns <- nodes']

shrinkScheduledMsgs
    :: ( IsCrypto c
       , Ord s
       )
    => Set (Scheduled c s)
    -> [Set (Scheduled c s)]
shrinkScheduledMsgs msgs =
    [Set.filter (not . isMsg) msgs <> ms | ms <- shrinkedMsgs]
  where
    shrinkedMsgs =
        shrinkMapBy
            Set.fromList
            Set.toList
            (shrinkList shrinkNothing)
            (Set.filter isMsg msgs)

-- | Filter messages where the sender is not part of the network. Used after
-- shrinking the node list.
filterNetwork :: TestNetwork c s a -> Maybe (TestNetwork c s a)
filterNetwork tn@TestNetwork{..} =
    if null scheduled
       then Nothing
       else Just tn { tnScheduled = scheduled }
  where
    scheduled = Set.filter condition tnScheduled
    condition msg =
        maybe True (`Map.member` tnNodes) (scheduledReceiver msg)

--------------------------------------------------------------------------------

-- | Splits a list into length-@n@ pieces. If @n@ is @<= 0@, returns an infinite
-- list of empty lists.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0     = take n l : chunksOf n (drop n l)
  | otherwise = repeat []
