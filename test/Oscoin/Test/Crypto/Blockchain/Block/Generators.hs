module Oscoin.Test.Crypto.Blockchain.Block.Generators
    ( -- * Generating genereric blocks
      genBlockFrom
    , genBlockWith
    , genStandaloneBlock

    , genSeal
    , genPoWSeal
    , genDifficulty

    -- * Generating Nakamoto blocks
    , genNakamotoBlockFrom
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Telemetry (tracing_)
import           Oscoin.Time

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()
import           Test.QuickCheck


-- | Generates an arbitrary block where the parent hash is randomly generated.
--
-- To create properly linked blockchains use 'genBlockFrom' or
-- 'genBlockchainFrom'.
genStandaloneBlock
    :: forall c tx s.
       ( IsCrypto c
       , Serialise s
       , Serialise tx
       , Arbitrary s
       , Arbitrary tx
       )
    => Gen (Block c tx s)
genStandaloneBlock = do
    txs :: [tx] <- arbitrary
    timestamp <- arbitrary
    diffi <- genDifficulty
    prevHash <- arbitrary
    stateHash <- arbitrary
    blockSeal <- arbitrary

    let header = (emptyHeader :: BlockHeader c Unsealed)
               { blockPrevHash   = prevHash
               , blockDataHash   = hashTxs txs
               , blockStateHash  = stateHash
               , blockTimestamp  = timestamp
               , blockSeal
               , blockTargetDifficulty = diffi
               }
    pure $ mkBlock header txs


-- | The difficulty is calculated with random swings with a factor of 4(**).
-- (**) This is /almost/ true: in particular, we decrease the difficulty of a
-- factor of 4000 to make sure we generate a wider variety of scores in our
-- tests, for example:
--
--     Chain Score (100 in total):
--     34% >50000 score
--     24% 5000-50000 score
--     16% 0 score
--     14% 500-5000 score
--     12% 0-10 score
--
genDifficultyFrom :: Difficulty -> Gen Difficulty
genDifficultyFrom (fromDifficulty -> prevDifficulty) =
    let (lessDifficulty :: Integer) = ceiling (fromIntegral prevDifficulty / 4000.0 :: Double)
        (moreDifficulty :: Integer) = ceiling (fromIntegral prevDifficulty * 4.0 :: Double)
    in frequency [ (20, pure (unsafeDifficulty prevDifficulty))
                 , (60, encodeDifficulty <$> choose (1, lessDifficulty))
                 , (20, (encodeDifficulty <$> choose (lessDifficulty, moreDifficulty)) `suchThat` noOverflow)
                 ]
  where noOverflow d = d <= maxDifficulty


-- | Generates a random 'Difficulty' without considering the previous one.
genDifficulty :: Gen Difficulty
genDifficulty =
    (unsafeDifficulty . getPositive <$> arbitrary) `suchThat` noOverflow
  where noOverflow d = d <= maxDifficulty


-- | Generates a block that satisfies the following properties
--
-- * The block contains the provided transactions and has a correct
-- 'blockDataHash'.
-- * 'blockStateHash' matches the hash of the provided state.
-- * 'blockPrevHash' matches the hash of the provided parent block.
-- * 'blockTargetDifficulty' follows 'genDifficultyFrom' using the
--   parent block.
-- * 'blockTimestamp' is larger than the parent block’s timestamp
genBlockWith
    :: forall c s tx state.
       ( Arbitrary s
       , Serialise s
       , Serialise tx
       , IsCrypto c
       , Crypto.Hashable c state
       )
    => Block c tx s
    -> [tx]
    -> state
    -> Gen (Block c tx s)
genBlockWith parentBlock txs st = do
    let prevHeader = blockHeader parentBlock
    elapsed    <- choose (2750 * seconds, 3250 * seconds)
    blockSeal  <- arbitrary
    blockDiffi <- genDifficultyFrom (blockTargetDifficulty prevHeader)
    let header = (emptyHeader :: BlockHeader c Unsealed)
               { blockHeight           = succ . blockHeight $ prevHeader
               , blockPrevHash         = headerHash prevHeader
               , blockDataHash         = hashTxs txs
               , blockStateHash        = hashState st
               , blockSeal
               , blockTimestamp        = blockTimestamp prevHeader `timeAdd` elapsed
               , blockTargetDifficulty = blockDiffi
               }
    pure $ mkBlock header txs

-- | Generates an arbitrary but valid block, linked against the input
-- one. Uses 'genBlockWith' with an arbitrary set of transactions and
-- an arbitrary state.
genBlockFrom
    :: ( IsCrypto c
       , Arbitrary tx
       , Arbitrary s
       , Serialise s
       , Serialise tx
       )
    => Block c tx s
    -> Gen (Block c tx s)
genBlockFrom parentBlock = do
    txs <- arbitrary
    st :: Word8 <- arbitrary
    genBlockWith parentBlock txs st

genSeal :: (Arbitrary s) => Gen (Sealed c s)
genSeal = SealedWith <$> arbitrary

genPoWSeal :: Gen (Sealed c Nakamoto.PoW)
genPoWSeal = SealedWith . Nakamoto.PoW <$> arbitrary

{------------------------------------------------------------------------------
  Generating Nakamoto's blocks
------------------------------------------------------------------------------}

genNakamotoBlockWith
    :: forall c tx.
       ( Serialise tx
       , IsCrypto c
       )
    => NonEmpty (Block c tx (Sealed c Nakamoto.PoW))
    -- ^ A non-empty list of blocks preceeding this one, needed for an
    -- accurate difficulty calculation.
    -> [tx]
    -> Gen (Block c tx (Sealed c Nakamoto.PoW))
genNakamotoBlockWith prefix@(parent:|_) txs = do
    let prevHeader = blockHeader parent
    elapsed    <- choose (60 * seconds, 120 * seconds)
    blockState <- arbitrary :: Gen Word8
    blockSeal  <- genPoWSeal
    blockDiffi <- pure $ tracing_ (Nakamoto.chainDifficulty (toList prefix))
    let header = (emptyHeader :: BlockHeader c Unsealed)
               { blockHeight           = succ (blockHeight prevHeader)
               , blockPrevHash         = headerHash prevHeader
               , blockDataHash         = hashTxs txs
               , blockStateHash        = hashState blockState
               , blockSeal
               , blockTimestamp        = blockTimestamp prevHeader `timeAdd` elapsed
               , blockTargetDifficulty = blockDiffi
               }
    pure $ mkBlock header txs

genNakamotoBlockFrom
    :: ( Arbitrary tx
       , Serialise tx
       , IsCrypto c
       )
    => NonEmpty (Block c tx (Sealed c Nakamoto.PoW))
    -> Gen (Block c tx (Sealed c Nakamoto.PoW))
genNakamotoBlockFrom ancestors = do
    txs <- arbitrary
    genNakamotoBlockWith ancestors txs
