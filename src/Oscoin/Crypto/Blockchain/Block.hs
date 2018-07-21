module Oscoin.Crypto.Blockchain.Block
    ( Block(..)
    , BlockHash
    , BlockHeader(..)
    , Difficulty
    , Height
    , Orphan
    , toOrphan
    , block
    , mkBlock
    , linkBlock
    , genesisBlock
    , isGenesisBlock
    , validateBlock
    , headerHash
    , blockHash
    , emptyHeader
    , hashTx
    , hashTxs
    ) where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash
import           Oscoin.Consensus.Evaluator (Evaluator, evals)

import           Data.Bifunctor (Bifunctor(..))
import           Data.Binary (Binary(..), encode)
import           Crypto.Hash (hashlazy)
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
    , blockDataHash   :: Hash
    , blockStateHash  :: Hash
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

-- | Represents an orphan state @s@. Blocks of type @Block tx (Orphan s)@ are
-- considered orphan blocks. The type @s -> Maybe s@ represents a function from
-- a parent state @s@ to a new state @s@ after a block is evaluated. Thus, we
-- can say that orphan blocks have an unapplied state function which produces
-- a state when they are linked to a parent.
type Orphan s = s -> Maybe s

-- | The hash of a block.
type BlockHash = Hashed (BlockHeader ())

-- | Block. @tx@ is the type of transaction stored in this block.
data Block tx s = Block
    { blockHeader :: BlockHeader s
    , blockData   :: Seq tx
    } deriving (Show, Generic, Functor, Foldable, Traversable)

deriving instance {-# OVERLAPPING #-} Eq tx => Eq (Block tx ())
deriving instance {-# OVERLAPPING #-} Ord tx => Ord (Block tx ())

instance {-# OVERLAPPABLE #-} (Eq tx)  => Eq (Block tx s) where
    (==) a b = map (const ()) a == map (const ()) b

instance {-# OVERLAPPABLE #-} (Ord tx)  => Ord (Block tx s) where
    (<=) a b = map (const ()) a <= map (const ()) b

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
    :: (Foldable t, Binary tx, Default s)
    => BlockHash
    -> Timestamp
    -> t tx
    -> Block tx s
block prev t txs =
    Block
        emptyHeader
            { blockPrevHash   = prev
            , blockTimestamp  = t
            , blockDataHash   = hashTxs txs
            , blockStateHash  = zeroHash
            , blockState      = def
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

genesisBlock :: (Foldable t, Binary tx, Default s) => Timestamp -> t tx -> Block tx s
genesisBlock t xs =
    block (toHashed zeroHash) t xs

isGenesisBlock :: Block tx s -> Bool
isGenesisBlock blk =
    (blockPrevHash . blockHeader) blk == toHashed zeroHash

toOrphan :: Evaluator s tx () -> Block tx s' -> Block tx (Orphan s)
toOrphan eval blk =
    second (const (\s -> evals (blockData blk) s eval)) blk

blockHash :: Block tx s -> BlockHash
blockHash blk = headerHash (blockHeader blk)

linkBlock :: Monad m => Block tx s -> Block tx (s -> m t) -> m (Block tx t)
linkBlock (blockState . blockHeader -> s) = traverse ($ s)

hashTxs :: (Foldable t, Binary tx) => t tx -> Hash
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
