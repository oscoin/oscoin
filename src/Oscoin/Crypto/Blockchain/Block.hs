module Oscoin.Crypto.Blockchain.Block where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash

import           Data.Bifunctor (Bifunctor(..))
import           Data.Binary (Binary(..), encode)
import           Crypto.Hash (hashlazy, Digest)
import qualified Crypto.Hash as Crypto
import qualified Crypto.Hash.MerkleTree as Merkle
import           Data.ByteString.Lazy (toStrict)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

-- | Block difficulty.
type Difficulty = Integer

-- | Block height.
type Height = Integer

-- | Block header.
data BlockHeader s = BlockHeader
    { blockPrevHash   :: Hashed (BlockHeader ())
    , blockDataHash   :: Digest HashAlgorithm
    , blockStateHash  :: Digest HashAlgorithm
    , blockState      :: s
    , blockTimestamp  :: Timestamp
    , blockDifficulty :: Difficulty
    , blockNonce      :: Word32
    } deriving (Show, Generic, Functor, Foldable, Traversable)

deriving instance {-# OVERLAPPING #-} Eq (BlockHeader ())
deriving instance {-# OVERLAPPING #-} Ord (BlockHeader ())

instance {-# OVERLAPPABLE #-} Eq (BlockHeader s) where
    (==) a b = map (const ()) a == map (const ()) b

instance Binary (BlockHeader ())

instance Hashable (BlockHeader ()) where
    hash = hashBinary

-- | Create an empty block header.
emptyHeader :: BlockHeader ()
emptyHeader = BlockHeader
    { blockPrevHash = toHashed zeroHash
    , blockDataHash = zeroHash
    , blockStateHash = zeroHash
    , blockState = ()
    , blockTimestamp = 0
    , blockDifficulty = 0
    , blockNonce = 0
    }

headerHash :: BlockHeader s -> Hashed (BlockHeader ())
headerHash =
    hash . map (const ())

-- TODO(alexis): Document.
type Orphan s = s -> Maybe s

-- TODO(alexis): Move this somewhere.
type Root = ()

-- | Block. @tx@ is the type of transaction stored in this block.
data Block tx s = Block
    { blockHeader :: BlockHeader s
    , blockData   :: Seq tx
    } deriving (Show, Generic, Functor, Foldable, Traversable)

deriving instance {-# OVERLAPPING #-} Eq tx => Eq (Block tx ())
deriving instance {-# OVERLAPPING #-} Ord tx => Ord (Block tx ())

instance {-# OVERLAPPABLE #-} (Eq tx)  => Eq (Block tx s) where
    (==) a b = second (const ()) a == second (const ()) b

instance {-# OVERLAPPABLE #-} (Ord tx)  => Ord (Block tx s) where
    (<=) a b = second (const ()) a <= second (const ()) b

instance (Binary tx) => Binary (Block tx ())

instance Bifunctor Block where
    first f b = b { blockData = f <$> blockData b }
    second f b = b { blockHeader = f <$> blockHeader b }

instance Bitraversable Block where
    bitraverse f g blk = go <$> traverse f (blockData blk)
                            <*> g (blockState $ blockHeader blk)
      where
        go a b = blk { blockHeader = h b, blockData = a }
        h a = (blockHeader blk) { blockState = a }

instance Bifoldable Block where
    bifoldMap f g blk =
        mappend (foldMap f a) (g b)
      where
        a = blockData blk
        b = blockState (blockHeader blk)

validateBlock :: Block tx s -> Either Error (Block tx s)
validateBlock = Right

block
    :: (Foldable t, Binary tx)
    => Hashed (BlockHeader ())
    -> Timestamp
    -> t tx
    -> Block tx Root
block prev t txs =
    Block
        emptyHeader
            { blockPrevHash   = prev
            , blockTimestamp  = t
            , blockDataHash   = hashTxs txs
            , blockStateHash  = zeroHash
            , blockState      = ()
            , blockDifficulty = 0
            }
        (Seq.fromList (toList txs))

mkBlock
    :: Foldable t
    => BlockHeader s
    -> t tx
    -> Block tx s
mkBlock header txs =
    Block header (Seq.fromList (toList txs))

genesisBlock :: (Foldable t, Binary tx) => Timestamp -> t tx -> Block tx Root
genesisBlock t xs =
    block (toHashed zeroHash) t xs

isGenesisBlock :: Block s a -> Bool
isGenesisBlock blk =
    (blockPrevHash . blockHeader) blk == toHashed zeroHash

hashTxs :: (Foldable t, Binary tx) => t tx -> Digest HashAlgorithm
hashTxs txs
    -- TODO: Get rid of merkle-tree dependency, or create our own that doesn't
    -- depend on protolude.
    -- TODO: Needs to return `Hashed (t tx)` or something.
    | null txs = zeroHash
    | otherwise =
        -- TODO(alexis): We shouldn't be double hashing here, but 'mtHash'
        -- gives us a SHA256 which we can't use.
        Crypto.hash
        . Merkle.mtHash
        . Merkle.mkMerkleTree
        $ map (toStrict . encode) (toList txs)

hashTx :: Binary tx => tx -> Hashed tx
hashTx tx =
    toHashed (hashlazy (encode tx))
