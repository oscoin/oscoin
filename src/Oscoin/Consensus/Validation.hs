{- | Standalone validation functions which can be composed to form more
-- sophisticated ones.
-}
module Oscoin.Consensus.Validation
    ( validateParentHash
    , validateDifficulty
    , validateTimestamp
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (Difficulty(..))
import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Time

import           Control.Monad.Except (Except, throwError)

-- | Rejects a block if its parent's hash is not the same as the hash of
-- the input (parent) block.
validateParentHash
    :: Eq (Hash c)
    => Block c tx s
    -> Block c tx s
    -> Except (ValidationError c) ()
validateParentHash parent blk
    | h <- blockPrevHash (blockHeader blk)
    , h /= blockHash parent =
        throwError $ InvalidParentHash h
    | otherwise = pure ()

-- | Rejects a 'Block' if the actual difficulty is different from the
-- expected one.
validateDifficulty
    :: ([Block c tx s] -> Difficulty)
    -- ^ A function to calculate the 'Difficulty' of the input list of blocks.
    -> [Block c tx s]
    -> Block c tx s
    -> Except (ValidationError c) ()
validateDifficulty chainDifficulty prefix blk
    | actual <- blockTargetDifficulty (blockHeader blk)
    , expected <- chainDifficulty prefix
    , actual /= expected =
        throwError $ InvalidTargetDifficulty expected actual
    | otherwise = pure ()

-- | Rejects a 'Block' if its timestamp is less than the parent's one.
validateTimestamp
    :: Block c tx s
    -> Block c tx s
    -> Except (ValidationError c) ()
validateTimestamp parent blk
    | t < t' = throwError $ InvalidBlockTimestamp $ t - t'
    | otherwise = pure ()
  where
    t  = ts blk
    t' = ts parent
    ts = sinceEpoch . blockTimestamp . blockHeader

