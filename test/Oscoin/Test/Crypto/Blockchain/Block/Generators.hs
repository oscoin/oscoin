module Oscoin.Test.Crypto.Blockchain.Block.Generators
    ( -- * Generating genereric blocks
      genBlockFrom

    -- * Generating Nakamoto blocks
    , genNakamotoBlockFrom
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain
import           Oscoin.Time

import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary ()
import           Oscoin.Test.Time ()
import           Test.QuickCheck

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

-- | Generates an arbitrary but valid block, linked against the input one
-- and cointaining the txs specified as input.
genBlockWith :: (Arbitrary s, Serialise s, Serialise tx)
             => Block tx s
             -> [tx]
             -> Gen (Block tx s)
genBlockWith parentBlock txs =  do
    let prevHeader = blockHeader parentBlock
    elapsed    <- choose (2750 * seconds, 3250 * seconds)
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

--- | Generates an arbitrary but valid block, linked against the input one.
genBlockFrom :: (Arbitrary tx, Arbitrary s, Serialise s, Serialise tx)
             => Block tx s
             -> Gen (Block tx s)
genBlockFrom parentBlock = do
    txs <- arbitrary
    genBlockWith parentBlock txs

{------------------------------------------------------------------------------
  Generating Nakamoto's blocks
------------------------------------------------------------------------------}

genNakamotoBlockWith
    :: forall tx. (Serialise tx)
    => NonEmpty (Block tx Nakamoto.PoW)
    -- ^ A non-empty list of blocks preceeding this one, needed for an
    -- accurate difficulty calculation.
    -> [tx]
    -> Gen (Block tx Nakamoto.PoW)
genNakamotoBlockWith prefix@(parent:|_) txs = do
    let prevHeader = blockHeader parent
    elapsed    <- choose (60 * seconds, 120 * seconds)
    blockState <- arbitrary :: Gen Word8
    blockSeal  <- arbitrary
    blockDiffi <- pure $ Nakamoto.chainDifficulty (toList prefix)
    let header = emptyHeader
               { blockPrevHash         = headerHash prevHeader
               , blockDataHash         = hashTxs txs
               , blockStateHash        = hashState blockState
               , blockSeal
               , blockTimestamp        = blockTimestamp prevHeader `timeAdd` elapsed
               , blockTargetDifficulty = blockDiffi
               }
    pure $ mkBlock header txs

genNakamotoBlockFrom
    :: (Arbitrary tx, Serialise tx)
    => NonEmpty (Block tx Nakamoto.PoW)
    -> Gen (Block tx Nakamoto.PoW)
genNakamotoBlockFrom ancestors = do
    txs <- arbitrary
    genNakamotoBlockWith ancestors txs
