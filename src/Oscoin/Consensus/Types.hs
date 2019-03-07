{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Consensus.Types
    ( ChainScoreFn
    , Consensus(..)
    , Validate
    , ValidationError(..)
    , Miner
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Time (Duration)

import qualified Generics.SOP as SOP

-- | Represents an abstract consensus protocol.
data Consensus c tx s m = Consensus
    { cScore    :: ChainScoreFn c tx s
    , cMiner    :: Miner c s m
    , cValidate :: Validate c tx s
    }

-- | Block validation function.
type Validate c tx s =
       [Block c tx (Sealed c s)]     -- ^ Ancestors of block to be validated.
    -> Block c tx (Sealed c s)       -- ^ Block to be validated.
    -> Either (ValidationError c) () -- ^ Either an error or @()@.

data ValidationError c =
      InvalidParentHash       (Crypto.Hash c)
    -- ^ Parent block hash doesn't match
    | InvalidDataHash         (Crypto.Hash c)
    -- ^ Block data hash doesn't match data
    | InvalidTargetDifficulty TargetDifficulty TargetDifficulty
    -- ^ Expected and actual target difficulty
    | InvalidBlockDifficulty  Difficulty       TargetDifficulty
    -- ^ Block difficulty doesn't match target
    | InvalidBlockTimestamp   Duration
    -- ^ Negative duration means the block is in the past, positive means too
    -- far in the future
    | InvalidBlockSize Int
    -- ^ The block exceeded the maximum block size.
    deriving (Generic)

deriving instance Show (Crypto.Hash c) => Show (ValidationError c)
deriving instance Eq (Crypto.Hash c)   => Eq (ValidationError c)

instance SOP.Generic (ValidationError c)
instance SOP.HasDatatypeInfo (ValidationError c)

-- | Block mining function.
type Miner c s m = forall tx.
    (Depth -> m [Block c tx (Sealed c s)])
    -> Block c tx Unsealed
    -> m (Maybe (Block c tx (Sealed c s)))
