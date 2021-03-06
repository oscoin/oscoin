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
    , findPoW
    , chainDifficulty
    , chainScore
    , blockScore
    , blockTime
    , validateBasic
    , validateFull
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Types
import           Oscoin.Consensus.Validation
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Block (Beneficiary, Difficulty(..))
import           Oscoin.Crypto.Hash (Hash, Hashable)
import           Oscoin.Telemetry (NotableEvent(..))
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import           Control.Monad.Except (liftEither, runExcept)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Crypto.Number.Serialize (os2ip)
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.List.NonEmpty as NonEmpty
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField (ToField)

-- | Target block time.
blockTime :: Duration
blockTime = 1 * minutes

-- | The default target difficulty at genesis.
defaultGenesisDifficulty :: Difficulty
defaultGenesisDifficulty =
    unsafeDifficulty 0x1d00ffff
    -- This is the original difficulty of Bitcoin at genesis.

-- | A PoW nonce.
type Nonce = Word32

-- | A PoW seal.
newtype PoW = PoW Nonce
    deriving (Eq, Ord, Show, Generic, FromField, ToField, Serialise)

-- | An empty (zero nonce) proof-of-work.
emptyPoW :: PoW
emptyPoW = PoW 0

-- | The Nakamoto consensus definition.
nakamotoConsensus
    :: ( AuthTree.MerkleHash (Hash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , ByteArrayAccess (BlockHash c)
       , Monad m
       , Serialise tx
       , Serialise (Beneficiary c)
       )
    => Telemetry.Tracer m
    -> Consensus c tx PoW m
nakamotoConsensus probed = Consensus
    { cScore = comparing chainScore
    , cMiner = mineNakamoto probed chainDifficulty
    , cValidate = validateFull
    }

-- | Validate a 'Block' in relation with a prefix.
validateFull
    :: ( ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , AuthTree.MerkleHash (Hash c)
       , Serialise tx
       , Serialise (Beneficiary c)
       )
    => Validate c tx PoW
validateFull [] blk =
    validateBasic blk
validateFull prefix@(parent:_) blk = runExcept $ do
    validateHeight     parent blk
    validateParentHash parent blk
    validateDifficulty chainDifficulty prefix blk
    validateTimestamp  parent blk
    validateBlockAge
    liftEither (validateBasic blk)
  where
    validateBlockAge
      | t - t' > 2 * hours =
          throwError (InvalidBlockTimestamp $ t' - t)
      | otherwise = pure ()

    t  = ts blk
    t' = ts parent
    ts = sinceEpoch . blockTimestamp . blockHeader

-- | Validate a 'Block' using only intrinsic block data.
validateBasic
    :: ( Serialise tx
       , Serialise (Beneficiary c)
       , ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , AuthTree.MerkleHash (Hash c)
       )
    => Block c tx (Sealed c PoW)     -- ^ Block to validate.
    -> Either (ValidationError c) () -- ^ Either a validation error, or success.
validateBasic b
    | h <- blockDataHash bHeader
    , h /= hashData (blockData b) =
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
       )
    => Telemetry.Tracer m
    -> (forall tx. [Block c tx (Sealed c PoW)] -> Difficulty)
    -> Miner c PoW m
mineNakamoto probed difiFn getBlocks unsealedBlock = do
    blks <- getBlocks difficultyBlocks
    let diffy = difiFn blks
    when (currentDifficulty blks /= diffy) $
         probed $ Telemetry.traced (DifficultyAdjustedEvent diffy (currentDifficulty blks)) ()

    let bh = (blockHeader unsealedBlock) { blockTargetDifficulty = diffy }
        candidateBlock = unsealedBlock { blockHeader = bh }

    pure $ (`sealBlock` candidateBlock) <$> findPoW bh

  where
    currentDifficulty []      = blockTargetDifficulty . blockHeader $ unsealedBlock
    currentDifficulty (blk:_) = blockTargetDifficulty . blockHeader $ blk

-- | Find a 'PoW' such that the header sealed with the proof of work
-- satisfies the header’s 'blockTargetDifficulty'. Returns 'Nothing' if
-- no such 'PoW' exists.
findPoW
    :: forall c s.
       ( ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       )
    => BlockHeader c s
    -> Maybe PoW
findPoW unsealedHeader = findPowFrom (PoW 0)
  where
    findPowFrom :: PoW -> Maybe PoW
    findPowFrom pow@(PoW nonce)
        | hasPoW (sealHeader pow unsealedHeader) = Just pow
        | nonce < maxBound            = findPowFrom (PoW (nonce + 1))
        | otherwise                   = Nothing

    sealHeader :: PoW -> BlockHeader c s -> BlockHeader c (Sealed c PoW)
    sealHeader seal hdr =
        hdr { blockSeal = SealedWith seal }

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

-- | Calculate the difficulty of a blockchain. Blocks are ordered newest-first.
chainDifficulty :: [Block c tx s] -> Difficulty
chainDifficulty [] = -- FIXME(adn) Use 'NewestFirst' here.
    minDifficulty
chainDifficulty (NonEmpty.fromList -> blks)
    | adjustmentRequired =
        let newDifficulty =
                encodeDifficulty $ fst (decodeDifficulty currentDifficulty)
                                 * fromIntegral targetElapsed
                                 `div` toInteger actualElapsed
        in newDifficulty
    | otherwise =
        prevDifficulty
  where

    -- The difficulty is adjusted every 'difficultyBlocks' blocks, and only if we do have
    -- 2016 blocks at hand to compute the new one. As the 'height' of genesis
    -- is 0, we need to add 1 to the value returned by 'blockHeight', to be
    -- sure to trigger an adjustment every 'difficultyBlocks' blocks.
    adjustmentRequired =
        (bHeight (NonEmpty.head blks) + 1) `mod` fromIntegral difficultyBlocks == 0
        && NonEmpty.length blks == fromIntegral difficultyBlocks

    bHeight           = blockHeight . blockHeader
    blocksConsidered  = fromIntegral difficultyBlocks
    prevDifficulty    = blockTargetDifficulty . blockHeader $ NonEmpty.head blks

    range             = NonEmpty.fromList $ NonEmpty.take blocksConsidered blks

    rangeStart        = blockHeader $ NonEmpty.last range
    rangeEnd          = blockHeader $ NonEmpty.head range

    actualElapsed     = blockTimestamp rangeEnd `timeDiff` blockTimestamp rangeStart
    targetElapsed     = fromIntegral blocksConsidered * blockTime

    currentDifficulty = blockTargetDifficulty rangeEnd
