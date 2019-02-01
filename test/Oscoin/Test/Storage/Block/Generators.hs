{- | Drop-in replacement for 'Oscoin.Test.Crypto.Blockchain.Arbitrary', coded
   separately for now to not cause too much breakage for tests relying on the
   fact chains are small.
-}

module Oscoin.Test.Storage.Block.Generators
    ( ForkParams(..)
    , genBlockFrom
    , genBlockchainFrom
    , genOrphanChainsFrom
    , genChainEvents
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import           GHC.Natural

import           Oscoin.Crypto.Blockchain
import           Oscoin.Time

import           Test.QuickCheck

data ForkParams = ForkParams
    { forkBranchingFactor :: Natural
    -- ^ Currently not used, but it can determine the /branching factor/
    -- for the fork, i.e. if this fork can generate other forks.
    , forkMaxLength       :: Natural
    -- ^ The maximum size for the generated fork.
    , forkNumber          :: Natural
    -- ^ The number of desired forks to generate.
    -- For example, a @forkProbability@ of 3 will ensure that /at most/
    -- (but there could be less) forks will be generated.
    }

data ChainEvent =
        DoNotFork
      | ForkChain
      deriving Show

genChainEvents :: Int
               -- ^ The size of the input chain.
               -> ForkParams
               -> Gen [ChainEvent]
genChainEvents inputChainSize ForkParams{..} = do
    doFork   <- vectorOf (fromIntegral forkNumber) (pure ForkChain)
    dontFork <- vectorOf (inputChainSize - fromIntegral forkNumber) (pure DoNotFork)
    shuffle (doFork ++ dontFork)

-- | The difficulty is calculated with random swings with a factor of 4,
-- as described in https://github.com/oscoin/oscoin/issues/344
genDifficultyFrom :: Difficulty -> Gen Difficulty
genDifficultyFrom (fromDifficulty -> prevDifficulty) =
    let (lessDifficulty :: Integer) = ceiling (fromIntegral prevDifficulty / 4.0 :: Double)
        (moreDifficulty :: Integer) = ceiling (fromIntegral prevDifficulty * 4.0 :: Double)
    in frequency [ (20, pure (Difficulty prevDifficulty))
                 , (60, fromIntegral <$> choose (1, lessDifficulty))
                 , (20, fromIntegral <$> choose (lessDifficulty, moreDifficulty))
                 ]

genBlockFrom :: (Arbitrary tx, Arbitrary s, Serialise s, Serialise tx)
             => Block tx s
             -> Gen (Block tx s)
genBlockFrom parentBlock =  do
    let prevHeader = blockHeader parentBlock
    elapsed    <- choose (2750 * seconds, 3250 * seconds)
    txs        <- listOf arbitrary
    blockState <- arbitrary :: Gen Word8
    blockSeal  <- arbitrary
    blockDiffi <- genDifficultyFrom (blockTargetDifficulty prevHeader)
    let header = emptyHeader
               { blockPrevHash         = headerHash prevHeader
               , blockDataHash         = hashTxs txs
               , blockStateHash        = hashState blockState
               , blockSeal
               , blockTimestamp        = blockTimestamp prevHeader `timeAdd` elapsed
               , blockTargetDifficulty = blockDiffi
               }
    pure $ mkBlock header txs

-- | Generates a 'Blockchain' from the input 'Block' (included in the resulting
-- chain).
genBlockchainFrom :: (Arbitrary tx, Arbitrary s, Serialise s, Serialise tx)
                  => Block tx s
                  -> Gen (Blockchain tx s)
genBlockchainFrom parentBlock = sized $ \n -> do
    foldM (\chain _ -> do
               newTip <- genBlockFrom (tip chain)
               pure (newTip |> chain)
          ) (unsafeToBlockchain [parentBlock]) [1 .. n]

-- | Generates a bunch of chain \"candidates\" to be used in more articulated
-- tests wanting to assess the resilience of fork selection. Here is the
-- general idea: traverse the input blockchain and for each block, with
-- random probability, branch off, creating a fork. This won't be a proper
-- fork, as we will deliberately create orphan chains, where we will need a
-- \"missing link\" in order to hook the orphan chain to the adopted one.
-- Return each and every orphan chain together with the \"missing link\"
-- necessary for the fork-selection algorithm to kick in.
-- Example (ghci session):
--
-- Ok, 134 modules loaded.
-- > let forkParams = ForkParams 0 5 3
-- > (myChain :: Blockchain Text ()) <- generate $ resize 9 (genBlockchainFrom (emptyGenesisBlock epoch))
-- > (orphans :: [(Blockchain Text (), Block Text ())]) <- generate $ genOrphanChainsFrom forkParams myChain
-- > showChainDigest myChain
-- "[EWokaLC (p:1111111) @0ns] <- [AxPcHbF (p:EWokaLC) @48m] <-
--  [76V2FAc (p:AxPcHbF) @1h] <- [YxLsVxJ (p:76V2FAc) @2h] <-
--  [5At4Aku (p:YxLsVxJ) @3h] <- [GACXY9Z (p:5At4Aku) @4h] <-
--  [AEW78LF (p:GACXY9Z) @5h] <- [FJSpSvu (p:AEW78LF) @6h] <-
-- [7zUXruW (p:FJSpSvu) @6h] <- [GGwLhg3 (p:7zUXruW) @7h]"
-- > Prelude.map (bimap showChainDigest showBlockDigest) orphans
-- [
--     (
--       "[9kGRVVf (p:G5jzmbW) @9h] <- [HjaQguf (p:9kGRVVf) @10h] <- [3R1XFCc (p:HjaQguf) @11h] <- [Bm2ETah (p:3R1XFCc) @11h]"
--     ,"[G5jzmbW (p:GGwLhg3) @8h]"
--   )
--   ,(
--       "[JCnXTHy (p:5RsGYYi) @3h] <- [6LPu23A (p:JCnXTHy) @4h] <- [DQRjgLS (p:6LPu23A) @4h] <- [Bdzwgr9 (p:DQRjgLS) @5h]"
--     ,"[5RsGYYi (p:76V2FAc) @2h]"
--   )
--   ,(
--       "[6eX4ctN (p:J1b4MKm) @1h] <- [H5k1K9A (p:6eX4ctN) @2h] <- [3oiWxf5 (p:H5k1K9A) @3h] <- [GTsUC2w (p:3oiWxf5) @4h]"
--     ,"[J1b4MKm (p:EWokaLC) @47m]"
--   )
-- ]
--
-- Note how the system generated 3 forks (as instructed). For the first one,
-- the \"missing link\" is block with hash @G5jzmbW@, which parent is
-- @GGwLhg3@, which is on the main chain.
--
genOrphanChainsFrom :: forall tx s. (Arbitrary tx, Arbitrary s, Serialise s, Serialise tx)
                    => ForkParams
                    -> Blockchain tx s
                    -- ^ The input (adopted) chain.
                    -> Gen [(Blockchain tx s, Block tx s)]
                    -- ^ A list of potential orphans together with
                    -- the missing link that, upon insertion, will
                    -- yield a fork.
genOrphanChainsFrom forkParams@ForkParams{..} inputChain = do
    let genesisFirst = reverse . toList . blocks $ inputChain
    events <- genChainEvents (length genesisFirst) forkParams
    go (zip events genesisFirst) [] -- Starts with genesis first
    where
        go :: [(ChainEvent, Block tx s)]
           -> [(Blockchain tx s, Block tx s)]
           -> Gen [(Blockchain tx s, Block tx s)]
        go [] !acc = pure acc
        go ((event,x):xs) !acc = do
            case event of
              DoNotFork -> go xs acc
              ForkChain -> do
                  forkSize <- choose ( min 2 (fromIntegral forkMaxLength)
                                     , fromIntegral forkMaxLength
                                     )
                  fork <- resize forkSize (genBlockchainFrom x)
                  let (blks, orphanChain) = fmap (unsafeToBlockchain . reverse)
                                          . splitAt 1
                                          . drop 1 -- drop the tip, which is 'x'.
                                          . reverse
                                          . blocks
                                          $ fork
                  -- NOTE(adn) Ignoring for now more complicated scenarios
                  -- where we could have a fork of a fork.
                  case blks of
                    [missingBlock] -> go xs ((orphanChain, missingBlock) : acc)
                    _ -> go xs acc
