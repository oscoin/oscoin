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

blockTime :: Duration
blockTime = 1 * seconds

-- | The default difficulty at genesis.
defaultGenesisDifficulty :: Difficulty
defaultGenesisDifficulty =
    unsafeDifficulty 0x1d00ffff
    -- This is the original difficulty of Bitcoin at genesis.

minGenesisDifficulty :: Difficulty
minGenesisDifficulty = minDifficulty

-- | A PoW nonce.
type Nonce = Word32

-- | A PoW seal.
newtype PoW = PoW Nonce
    deriving (Eq, Ord, Show, Generic, FromField, ToField, ToJSON, FromJSON, Serialise)

-- | An empty (zero nonce) proof-of-work.
emptyPoW :: PoW
emptyPoW = PoW 0

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

chainScore :: Blockchain c tx PoW -> Int
chainScore = fromIntegral . height

hasPoW
    :: ( ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c s)
       )
    => BlockHeader c s -> Bool
hasPoW header =
    difficulty header < blockTargetDifficulty header

difficulty
    :: ( ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c s)
       )
    => BlockHeader c s
    -> Difficulty
difficulty = encodeDifficulty . os2ip . headerHash

-- | Number of blocks to consider for difficulty calculation. Roughly 2 weeks
-- withh a 10 min block time.
difficultyBlocks :: Depth
difficultyBlocks = 2016

-- | Calculate the difficulty of a blockchain.
chainDifficulty :: [Block c tx s] -> Difficulty
chainDifficulty [] =
    minGenesisDifficulty
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
