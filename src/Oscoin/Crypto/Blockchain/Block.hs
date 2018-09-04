module Oscoin.Crypto.Blockchain.Block
    ( Block(..)
    , BlockHash
    , BlockHeader(..)
    , Difficulty
    , Height
    , Orphan
    , toOrphan
    , mkBlock
    , linkBlock
    , genesisBlock
    , emptyGenesisBlock
    , isGenesisBlock
    , validateBlock
    , headerHash
    , blockHash
    , emptyHeader
    , hashTx
    , hashTxs
    , prettyBlock
    ) where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash
import           Oscoin.Consensus.Evaluator (Evaluator, EvalError, evals)

import qualified Prelude
import           Data.Bifunctor (Bifunctor(..))
import qualified Codec.Serialise as Serialise
import           Codec.Serialise (Serialise)
import           Crypto.Hash (hashlazy)
import qualified Crypto.Hash as Crypto
import qualified Crypto.Hash.MerkleTree as Merkle
import           Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Sequence as Seq
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Text.Printf
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
    (==) a b = void a == void b

instance Serialise (BlockHeader ())

instance Hashable (BlockHeader ()) where
    hash = hashSerial

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
    hash . void

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
    (==) a b = void a == void b

instance {-# OVERLAPPABLE #-} (Ord tx)  => Ord (Block tx s) where
    (<=) a b = void a <= void b

instance (Serialise tx) => Serialise (Block tx ())

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

mkBlock
    :: Foldable t
    => BlockHeader s
    -> t tx
    -> Block tx s
mkBlock header txs =
    Block header (Seq.fromList (toList txs))

genesisHeader :: (Foldable t, Serialise tx, Default s) => Timestamp -> t tx -> BlockHeader s
genesisHeader t txs = emptyHeader
    { blockDataHash  = hashTxs txs
    , blockTimestamp = t
    , blockStateHash = zeroHash
    , blockState     = def
    }

genesisBlock
    :: forall t tx s. (Foldable t, Serialise tx)
    => s
    -> Evaluator s tx ()
    -> Timestamp
    -> t tx
    -> Either [EvalError] (Block tx s)
genesisBlock s eval t xs =
    evalBlock s eval blk
  where
    blk = mkBlock (genesisHeader t xs) xs :: Block tx ()

emptyGenesisBlock
    :: forall tx s. (Serialise tx, Default s)
    => Timestamp
    -> Block tx s
emptyGenesisBlock t =
    mkBlock (genesisHeader t ([] :: [tx])) []

isGenesisBlock :: Block tx s -> Bool
isGenesisBlock blk =
    (blockPrevHash . blockHeader) blk == toHashed zeroHash

-- | Evaluate a block, setting its state @s@. Returns 'Nothing' if evaluation
-- failed.
evalBlock
    :: s                 -- ^ Input state
    -> Evaluator s tx () -- ^ Evaluator
    -> Block tx ()       -- ^ Block to evaluate
    -> Either [EvalError] (Block tx s)
evalBlock s eval blk =
    sequence $ blk $> evals (blockData blk) s eval

toOrphan :: Evaluator s tx () -> Block tx s' -> Block tx (Orphan s)
toOrphan eval blk =
    blk $> \s -> rightToMaybe (evals (blockData blk) s eval)

blockHash :: Block tx s -> BlockHash
blockHash blk = headerHash (blockHeader blk)

linkBlock :: Monad m => Block tx s -> Block tx (s -> m t) -> m (Block tx t)
linkBlock (blockState . blockHeader -> s) = traverse ($ s)

hashTxs :: (Foldable t, Serialise tx) => t tx -> Hash
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
        $ map (toStrict . Serialise.serialise) (toList txs)

hashTx :: Serialise tx => tx -> Hashed tx
hashTx tx =
    toHashed (hashlazy (Serialise.serialise tx))

prettyBlock :: (Hashable tx, Pretty tx) => Block tx s -> Maybe Int -> String
prettyBlock (Block bh@BlockHeader{..} txs) h = execWriter $ do
    tell $ printf "┍━%-6s━━ %s ━━┑\n" height (C8.unpack $ toHex $ headerHash bh)
    tell $ printf "│ prevHash:   %-64s │\n" (C8.unpack $ toHex blockPrevHash)
    tell $ printf "│ timestamp:  %-64d │\n" blockTimestamp
    tell $ printf "│ rootHash:   %-64s │\n" (C8.unpack $ toHex blockStateHash)
    tell $ printf "├────────%s─────────┤\n" (Prelude.replicate 61 '─')

    for_ (zip [0..Seq.length txs] (toList txs)) $ \(n, tx) -> do
        tell $ printf "│ %03d:  %-64s       │\n" n (C8.unpack $ toHex $ hash tx)
        tell $ printf "│ %-64s              │\n" (renderStrict . layoutCompact . pretty $ tx)
        tell $ printf "├────────%s──────────┤\n" (Prelude.replicate 60 '─')

    tell $ printf "└────────%s─────────┘\n" (Prelude.replicate 61 '─')
  where
    height = maybe "━━━━━━━" (\x -> " " ++ show x ++ " ") h
