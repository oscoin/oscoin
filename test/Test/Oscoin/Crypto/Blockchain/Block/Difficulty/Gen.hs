module Test.Oscoin.Crypto.Blockchain.Block.Difficulty.Gen
    ( genDifficulty
    , genTestableDifficulty
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block.Difficulty

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Generates a difficulty between 'minDifficulty' and
-- 'maxDifficulty'. Shrinks to towards 'minDifficulty'. The size
-- parameter reduces the maximum generated difficulty
genDifficulty :: Gen Difficulty
genDifficulty = encodeDifficulty <$> Gen.integral (Range.linear minDifficultyInt maxDifficultyInt)

-- | Generates a difficulty between 'minDifficulty' and @0x0FF...@.
-- This guarantees that it is fast but not trivial to generate a proof
-- of work for the generated difficulty.
genTestableDifficulty :: Gen Difficulty
genTestableDifficulty = encodeDifficulty <$> Gen.integral (Range.linear minDifficultyInt testableDifficultyInt)

testableDifficultyInt :: Integer
testableDifficultyInt = 0x0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

minDifficultyInt :: Integer
minDifficultyInt = fst $ decodeDifficulty minDifficulty

maxDifficultyInt :: Integer
maxDifficultyInt = fst $ decodeDifficulty maxDifficulty
