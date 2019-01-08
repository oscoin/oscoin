module Oscoin.Crypto.Blockchain.Block
    ( Block(..)
    , BlockHash
    , BlockHeader(..)
    , StateHash

    , Difficulty(..)
    , minDifficulty
    , easyDifficulty
    , parseDifficulty
    , prettyDifficulty
    , TargetDifficulty

    , Height
    , Depth
    , Score
    , Timestamp
    , mkBlock
    , emptyGenesisBlock
    , emptyGenesisFromState
    , genesisBlock
    , isGenesisBlock
    , headerHash
    , withHeader
    , blockScore
    , sealBlock
    , linkParent
    , emptyHeader
    , hashState
    , hashTxs
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Codec.Serialise.Decoding as Serialise
import qualified Codec.Serialise.Encoding as Serialise
import           Control.Monad (fail)
import qualified Crypto.Data.Auth.Tree as AuthTree
import           Crypto.Number.Serialize (i2osp, os2ip)
import           Data.Aeson
                 ( FromJSON(..)
                 , ToJSON(..)
                 , object
                 , withObject
                 , withText
                 , (.:)
                 , (.=)
                 )
import qualified Data.ByteString.BaseN as BaseN
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as Seq
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField (ToField)
import           GHC.Generics (Generic)
import           Numeric.Natural
import           Text.Show (Show(..))

-- | Block difficulty.
newtype Difficulty = Difficulty { fromDifficulty :: Integer }
    deriving (Show, Read, Eq, Ord, Num, Enum, Real, Integral, Serialise, FromField, ToField)

instance ToJSON Difficulty where
    toJSON = toJSON . prettyDifficulty

instance FromJSON Difficulty where
    parseJSON = withText "Difficulty" $ \t ->
        case parseDifficulty t of
            Just d  -> pure d
            Nothing -> fail "Error decoding difficulty"

prettyDifficulty :: Difficulty -> Text
prettyDifficulty =
     BaseN.encodedText . BaseN.encodeBase16 . i2osp . fromDifficulty

parseDifficulty :: Text -> Maybe Difficulty
parseDifficulty t =
    case BaseN.decodeBase16 $ encodeUtf8 t of
        Just d  -> Just $ Difficulty (os2ip d)
        Nothing -> Nothing

-- | The minimum difficulty.
minDifficulty :: Difficulty
minDifficulty =
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | An easy difficulty. About 24s per block on a single core.
easyDifficulty :: Difficulty
easyDifficulty =
    0x00000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | Minimum difficulty a 'Block' must have to be considered valid.
--
-- Invariant: 'Difficulty' must always be lower (harder) than 'TargetDifficulty' for
-- valid blocks.
type TargetDifficulty = Difficulty

-- | Block height.
type Height = Integer

-- | Block depth.
type Depth = Natural

-- | Block score.
type Score = Integer

-- | Block header.
data BlockHeader s = BlockHeader
    { blockPrevHash   :: Crypto.Hash
    , blockDataHash   :: Crypto.Hash
    , blockStateHash  :: Crypto.Hash
    , blockTimestamp  :: Timestamp
    , blockDifficulty :: TargetDifficulty
    , blockSeal       :: s
    } deriving (Show, Generic, Functor, Foldable, Traversable)

deriving instance Ord s => Ord (BlockHeader s)
deriving instance Eq s => Eq (BlockHeader s)

instance Serialise s => Serialise (BlockHeader s)

instance Serialise s => Crypto.Hashable (BlockHeader s) where
    hash = Crypto.hashSerial

instance ToJSON s => ToJSON (BlockHeader s) where
    toJSON BlockHeader{..} = object
        [ "parentHash" .= blockPrevHash
        , "timestamp"  .= blockTimestamp
        , "dataHash"   .= blockDataHash
        , "stateHash"  .= blockStateHash
        , "seal"       .= blockSeal
        , "difficulty" .= blockDifficulty
        ]

instance FromJSON s => FromJSON (BlockHeader s) where
  parseJSON = withObject "BlockHeader" $ \o -> do
        blockPrevHash   <- o .: "parentHash"
        blockTimestamp  <- o .: "timestamp"
        blockDataHash   <- o .: "dataHash"
        blockStateHash  <- o .: "stateHash"
        blockSeal       <- o .: "seal"
        blockDifficulty <- o .: "difficulty"

        pure BlockHeader{..}

-- | Create an empty block header.
emptyHeader :: BlockHeader ()
emptyHeader = BlockHeader
    { blockPrevHash = Crypto.zeroHash
    , blockDataHash = Crypto.zeroHash
    , blockStateHash = Crypto.zeroHash
    , blockSeal = ()
    , blockTimestamp = epoch
    , blockDifficulty = 0
    }

headerHash :: Serialise s => BlockHeader s -> BlockHash
headerHash =
    Crypto.fromHashed . Crypto.hash

withHeader :: Serialise s => Block tx a -> BlockHeader s -> Block tx s
withHeader blk h = blk { blockHeader = h, blockHash = headerHash h }

-- | Set the block parent hash of a block to the supplied parent.
linkParent
    :: Serialise s
    => Block tx s -- ^ The parent block
    -> Block tx s -- ^ The unlinked child block
    -> Block tx s -- ^ The newly-linked child
linkParent p blk =
    blk { blockHeader = header, blockHash = headerHash header }
  where
    header = (blockHeader blk) { blockPrevHash = blockHash p }

-- | The hash of a block.
type BlockHash = Crypto.Hash

-- | The hash of a state tree.
type StateHash = Crypto.Hash

-- | Block. @tx@ is the type of transaction stored in this block.
--
-- Nb. There is no instance for 'Functor' on 'Block' because updating the @s@
-- parameter would require the side-effect of updating the 'BlockHash' for
-- the update to be valid. Instead, use the 'sealBlock' function.
data Block tx s = Block
    { blockHeader :: BlockHeader s
    , blockHash   :: BlockHash
    , blockData   :: Seq tx
    } deriving (Show, Generic)

deriving instance (Eq tx, Eq s) => Eq (Block tx s)
deriving instance (Ord tx, Ord s) => Ord (Block tx s)

instance (Serialise tx, Serialise s) => Serialise (Block tx s) where
    encode Block{..} =
           Serialise.encodeListLen 4
        <> Serialise.encodeWord 0
        <> Serialise.encode blockHeader
        <> Serialise.encode blockHash
        <> Serialise.encode blockData

    decode = do
        Serialise.decodeListLenOf 4
        tag <- Serialise.decodeWord
        case tag of
            0 -> do
                !blockHeader <- Serialise.decode
                !blockHash   <- Serialise.decode
                !blockData   <- Serialise.decode

                if headerHash blockHeader /= blockHash
                   then fail "Error decoding block: hash does not match data"
                   else pure Block{..}
            _ ->
                fail "Error decoding block: unknown tag"

instance (Serialise s, ToJSON s, ToJSON tx) => ToJSON (Block tx s) where
    toJSON Block{..} = object
        [ "hash"   .= blockHash
        , "header" .= blockHeader
        , "data"   .= blockData
        ]

instance (Serialise s, FromJSON s, FromJSON tx) => FromJSON (Block tx s) where
  parseJSON = withObject "Block" $ \o -> do
        blockHeader <- o .: "header"
        blockData   <- o .: "data"
        blockHash   <- o .: "hash"

        if headerHash blockHeader /= blockHash
           then fail "Error decoding block: hash does not match data"
           else pure Block{..}

mkBlock
    :: (Foldable t, Serialise s)
    => BlockHeader s
    -> t tx
    -> Block tx s
mkBlock header txs =
    Block header (headerHash header) (Seq.fromList (toList txs))

emptyGenesisBlock :: Timestamp -> Block tx ()
emptyGenesisBlock blockTimestamp =
    mkBlock header []
  where
    header = emptyHeader { blockTimestamp }

emptyGenesisFromState :: Crypto.Hashable st => Timestamp -> st -> Block tx ()
emptyGenesisFromState blockTimestamp st =
    mkBlock header []
  where
    header = emptyHeader { blockTimestamp, blockStateHash = stHash }
    stHash = Crypto.fromHashed . Crypto.hash $ st

-- | Construct a sealed genesis block.
genesisBlock
    :: (Serialise s, Serialise tx, Crypto.Hashable st)
    => [tx] -> st -> s -> Timestamp -> Block tx s
genesisBlock txs st seal t =
    mkBlock header txs
  where
    header = emptyHeader
        { blockTimestamp = t
        , blockSeal = seal
        , blockStateHash = hashState st
        , blockDataHash = hashTxs txs
        }

isGenesisBlock :: Block tx s -> Bool
isGenesisBlock Block{..} =
    blockPrevHash blockHeader == Crypto.zeroHash

sealBlock :: Serialise s => s -> Block tx a -> Block tx s
sealBlock seal blk =
    blk' { blockHash = headerHash (blockHeader blk') }
  where
    blk' = blk { blockHeader = (blockHeader blk) { blockSeal = seal } }

blockScore :: Block tx s -> Integer
blockScore = fromDifficulty . blockDifficulty . blockHeader

hashState :: Crypto.Hashable st => st -> StateHash
hashState = Crypto.fromHashed . Crypto.hash

hashTxs :: (Foldable t, Serialise tx) => t tx -> Crypto.Hash
hashTxs (toList -> txs)
    | null txs = Crypto.zeroHash
    | otherwise =
          Crypto.toHash
        . AuthTree.merkleHash
        . AuthTree.fromList
        $ [(tx, mempty :: ByteString) | tx <-
            map (LBS.toStrict . Serialise.serialise) txs]
        -- Nb. Since our Merkle tree works with key-value pairs, but we're only
        -- really interested in the keys being present or absent for this use-case,
        -- we use the empty byte string as the value component.
