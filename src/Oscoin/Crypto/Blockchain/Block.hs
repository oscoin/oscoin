module Oscoin.Crypto.Blockchain.Block
    ( Block(..)
    , BlockHash
    , BlockHeader(..)
    , StateHash
    , Difficulty
    , Height
    , Timestamp
    , mkBlock
    , emptyGenesisBlock
    , emptyGenesisFromState
    , genesisBlock
    , validateBlock
    , headerHash
    , blockHash
    , emptyHeader
    , hashState
    , hashTxs
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Crypto.Hash.MerkleTree as Merkle
import           Data.Aeson
                 (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import           Data.Bifoldable (Bifoldable(..))
import           Data.Bifunctor (Bifunctor(..))
import           Data.Bitraversable (Bitraversable(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

-- | Block difficulty.
type Difficulty = Integer

-- | Block height.
type Height = Integer

-- | Block header.
data BlockHeader s = BlockHeader
    { blockPrevHash   :: Crypto.Hash
    , blockDataHash   :: Crypto.Hash
    , blockStateHash  :: Crypto.Hash
    , blockTimestamp  :: Timestamp
    , blockDifficulty :: Difficulty
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

-- | The hash of a block.
type BlockHash = Crypto.Hash

-- | The hash of a state tree.
type StateHash = Crypto.Hash

-- | Block. @tx@ is the type of transaction stored in this block.
data Block tx s = Block
    { blockHeader :: BlockHeader s
    , blockData   :: Seq tx
    } deriving (Show, Generic, Functor, Foldable, Traversable)

deriving instance (Eq tx, Eq s) => Eq (Block tx s)
deriving instance (Ord tx, Ord s) => Ord (Block tx s)

instance (Serialise tx, Serialise s) => Serialise (Block tx s)

instance Bifunctor Block where
    first f b = b { blockData = f <$> blockData b }
    second f b = b { blockHeader = f <$> blockHeader b }

instance Bitraversable Block where
    bitraverse f g blk = go <$> traverse f (blockData blk)
                            <*> g (blockSeal $ blockHeader blk)
      where
        go a b = blk { blockHeader = h b, blockData = a }
        h a = (blockHeader blk) { blockSeal = a }

instance Bifoldable Block where
    bifoldMap f g blk =
        mappend (foldMap f a) (g b)
      where
        a = blockData blk
        b = blockSeal (blockHeader blk)

instance (Serialise s, ToJSON s, ToJSON tx) => ToJSON (Block tx s) where
    toJSON b@Block{..} = object
        [ "hash"   .= blockHash b
        , "header" .= blockHeader
        , "data"   .= blockData
        ]

instance (FromJSON s, FromJSON tx) => FromJSON (Block tx s) where
  parseJSON = withObject "Block" $ \o -> do
        blockHeader <- o .: "header"
        blockData   <- o .: "data"

        pure Block{..}

validateBlock :: Block tx s -> Either Text (Block tx s)
validateBlock = Right

mkBlock
    :: Foldable t
    => BlockHeader s
    -> t tx
    -> Block tx s
mkBlock header txs =
    Block header (Seq.fromList (toList txs))

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
    :: (Serialise tx, Crypto.Hashable st)
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

blockHash :: Serialise s => Block tx s -> BlockHash
blockHash blk = headerHash (blockHeader blk)

hashState :: Crypto.Hashable st => st -> StateHash
hashState = Crypto.fromHashed . Crypto.hash

hashTxs :: (Foldable t, Serialise tx) => t tx -> Crypto.Hash
hashTxs txs
    -- TODO: Get rid of merkle-tree dependency, or create our own that doesn't
    -- depend on protolude.
    -- TODO: Needs to return `Hashed (t tx)` or something.
    | null txs = Crypto.zeroHash
    | otherwise =
        -- TODO(alexis): We shouldn't be double hashing here, but 'mtHash'
        -- gives us a SHA256 which we can't use.
        Crypto.fromHashed
        . Crypto.hash
        . Merkle.mtHash
        . Merkle.mkMerkleTree
        $ map (LBS.toStrict . Serialise.serialise) (toList txs)
