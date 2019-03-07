module Oscoin.Consensus.Nakamoto
    ( nakamotoConsensus
    , mineNakamoto
    , minDifficulty
    , easyDifficulty
    , defaultGenesisDifficulty
    , difficulty
    , PoW(..)
    , emptyPoW
    , hasPoW
    , chainDifficulty
    , chainScore
    , blockScore
    , blockTime
    , validateBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (Difficulty(..))
import           Oscoin.Crypto.Hash (Hash, Hashable)
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import qualified Crypto.Data.Auth.Tree.Internal as AuthTree
import           Crypto.Number.Serialize (os2ip)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.List.NonEmpty as NonEmpty
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField (ToField)

-- | Target block time.
blockTime :: Duration
blockTime = 1 * seconds

-- | The default target difficulty at genesis.
defaultGenesisDifficulty :: Difficulty
defaultGenesisDifficulty =
    unsafeDifficulty 0x1d00ffff
    -- This is the original difficulty of Bitcoin at genesis.

-- | A PoW nonce.
type Nonce = Word32

-- | A PoW seal.
newtype PoW = PoW Nonce
    deriving (Eq, Ord, Show, Generic, FromField, ToField, ToJSON, FromJSON, Serialise)

-- | An empty (zero nonce) proof-of-work.
emptyPoW :: PoW
emptyPoW = PoW 0

-- | The Nakamoto consensus definition.
nakamotoConsensus
    :: ( Eq (Hash c)
       , AuthTree.MerkleHash (Hash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , Hashable c (BlockHeader c Unsealed)
       , ByteArrayAccess (BlockHash c)
       , Monad m
       , Serialise tx
       )
    => Consensus c tx PoW m
nakamotoConsensus = Consensus
    { cScore = comparing chainScore
    , cMiner = mineNakamoto chainDifficulty
    , cValidate = validateBlock
    }

-- | Validate a 'Block' in relation with a prefix.
validateBlock
    :: ( Eq (Hash c)
       , ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , AuthTree.MerkleHash (Hash c)
       , Serialise tx
       )
    => Validate c tx PoW
validateBlock [] blk =
    validateBlock' blk
validateBlock prefix@(parent:_) blk
    | h <- blockPrevHash (blockHeader blk)
    , h /= blockHash parent =
        Left $ InvalidParentHash h
    | actual <- blockTargetDifficulty (blockHeader blk)
    , expected <- chainDifficulty prefix
    , actual /= expected =
        Left $ InvalidTargetDifficulty expected actual
    | t < t' =
        Left $ InvalidBlockTimestamp $ t - t'
    | t - t' > 2 * hours =
        Left $ InvalidBlockTimestamp $ t' - t
    | otherwise =
        validateBlock' blk
  where
    t  = ts blk
    t' = ts parent
    ts = sinceEpoch . blockTimestamp . blockHeader

-- | Validate a 'Block' using only intrinsic block data.
validateBlock'
    :: ( Eq (Hash c)
       , Serialise tx
       , ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , AuthTree.MerkleHash (Hash c)
       )
    => Block c tx (Sealed c PoW)     -- ^ Block to validate.
    -> Either (ValidationError c) () -- ^ Either a validation error, or success.
validateBlock' b
    | h <- blockDataHash bHeader
    , h /= hashTxs (blockData b) =
        Left $ InvalidDataHash h
    | not (hasPoW bHeader) =
        Left $ InvalidBlockDifficulty (difficulty bHeader)
                                      (blockTargetDifficulty bHeader)
    | otherwise =
        Right ()
  where
      bHeader = blockHeader b

-- | Try to mine a Nakamoto 'Block'. Returns 'Nothing' if all possible values
-- for the 'Nonce' have been tried without success.
mineNakamoto
    :: forall c m.
       ( Monad m
       , ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , Hashable c (BlockHeader c Unsealed)
       )
    => (forall tx. [Block c tx (Sealed c PoW)] -> Difficulty)
    -> Miner c PoW m
mineNakamoto difiFn getBlocks unsealedBlock = do
    blks <- getBlocks difficultyBlocks

    let bh = (blockHeader unsealedBlock) { blockTargetDifficulty = difiFn blks }
        candidateBlock = unsealedBlock { blockHeader = bh }

    pure $ (`sealBlock` candidateBlock) <$> mine bh (PoW 0)

  where
    mine :: BlockHeader c Unsealed -> PoW -> Maybe PoW
    mine hdr (PoW nonce)
        | hasPoW hdr        = Just (PoW nonce)
        | nonce < maxBound  = mine hdr (PoW (nonce + 1))
        | otherwise         = Nothing

-- | The block score is equivalent to the /average/ difficulty of computing
-- its proof-of-work.
blockScore :: Block c tx s -> Score
blockScore = fst . decodeDifficulty . blockTargetDifficulty . blockHeader

-- | Calculate the total score or \"weight\" of a chain.
chainScore :: Blockchain c tx PoW -> Score
chainScore = sum . map blockScore . blocks

-- | Check whether or not a 'BlockHeader' has a valid proof-of-work 'Seal'.
--
-- For this to be the case, the actual difficulty must be below the target
-- difficulty.
hasPoW
    :: ( ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c s)
       )
    => BlockHeader c s -> Bool
hasPoW header =
    difficulty header < blockTargetDifficulty header

-- | Calculate the actual difficulty of a block.
difficulty
    :: ( ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c s)
       )
    => BlockHeader c s
    -> Difficulty
difficulty = encodeDifficulty . os2ip . headerHash

-- | Number of blocks to consider for difficulty calculation. Roughly 2 weeks
-- with a 10 minute block time.
difficultyBlocks :: Depth
difficultyBlocks = 2016

-- | Calculate the difficulty of a blockchain.
chainDifficulty :: [Block c tx s] -> Difficulty
chainDifficulty [] =
    minDifficulty
chainDifficulty (NonEmpty.fromList -> blks)
    | NonEmpty.length blks `mod` fromIntegral difficultyBlocks == 0 =
        encodeDifficulty $ fst (decodeDifficulty currentDifficulty)
                         * fromIntegral targetElapsed
                         `div` toInteger actualElapsed
    | otherwise =
        prevDifficulty
  where
    blocksConsidered  = fromIntegral difficultyBlocks
    prevDifficulty    = blockTargetDifficulty . blockHeader $ NonEmpty.head blks

    range             = NonEmpty.fromList $ NonEmpty.take blocksConsidered blks

    rangeStart        = blockHeader $ NonEmpty.last range
    rangeEnd          = blockHeader $ NonEmpty.head range

    actualElapsed     = blockTimestamp rangeEnd `timeDiff` blockTimestamp rangeStart
    targetElapsed     = fromIntegral blocksConsidered * blockTime

    currentDifficulty = blockTargetDifficulty rangeEnd
