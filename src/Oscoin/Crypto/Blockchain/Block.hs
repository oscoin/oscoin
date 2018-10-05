module Oscoin.Crypto.Blockchain.Block
    ( Block(..)
    , BlockHash
    , BlockHeader(..)
    , Difficulty
    , Height
    , Timestamp
    , Orphan
    , mkBlock
    , linkBlock
    , emptyGenesisBlock
    , validateBlock
    , headerHash
    , blockHash
    , emptyHeader
    , hashTxs
    , prettyBlock
    ) where

import           Oscoin.Prelude
import qualified Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Time

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Control.Monad.Writer.CPS (execWriter, tell)
import qualified Crypto.Hash.MerkleTree as Merkle
import           Data.Aeson
                 (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import           Data.Bifoldable (Bifoldable(..))
import           Data.Bifunctor (Bifunctor(..))
import           Data.Bitraversable (Bitraversable(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Formatting ((%), (%.))
import qualified Formatting as Fmt
import           GHC.Generics (Generic)

-- | Block difficulty.
type Difficulty = Integer

-- | Block height.
type Height = Integer

-- | Block header.
data BlockHeader s = BlockHeader
    { blockPrevHash   :: Crypto.Hashed (BlockHeader ())
    , blockDataHash   :: Crypto.Hash
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

instance Crypto.Hashable (BlockHeader ()) where
    hash = Crypto.hashSerial

instance ToJSON (BlockHeader s) where
    toJSON BlockHeader{..} = object
        [ "parentHash" .= blockPrevHash
        , "timestamp"  .= blockTimestamp
        , "dataHash"   .= blockDataHash
        , "nonce"      .= blockNonce
        , "difficulty" .= blockDifficulty
        ]

instance FromJSON (BlockHeader ()) where
  parseJSON = withObject "BlockHeader" $ \o -> do
        blockPrevHash   <- o .: "parentHash"
        blockTimestamp  <- o .: "timestamp"
        blockDataHash   <- o .: "dataHash"
        blockNonce      <- o .: "nonce"
        blockDifficulty <- o .: "difficulty"
        blockState      <- pure ()

        pure BlockHeader{..}

-- | Create an empty block header.
emptyHeader :: BlockHeader ()
emptyHeader = BlockHeader
    { blockPrevHash = Crypto.toHashed Crypto.zeroHash
    , blockDataHash = Crypto.zeroHash
    , blockState = ()
    , blockTimestamp = epoch
    , blockDifficulty = 0
    , blockNonce = 0
    }

headerHash :: BlockHeader s -> Crypto.Hashed (BlockHeader ())
headerHash =
    Crypto.hash . void

-- | Represents an orphan state @s@. Blocks of type @Block tx (Orphan s)@ are
-- considered orphan blocks. The type @s -> Maybe s@ represents a function from
-- a parent state @s@ to a new state @s@ after a block is evaluated. Thus, we
-- can say that orphan blocks have an unapplied state function which produces
-- a state when they are linked to a parent.
type Orphan s = s -> Maybe s

-- | The hash of a block.
type BlockHash = Crypto.Hashed (BlockHeader ())

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

instance ToJSON tx => ToJSON (Block tx s) where
    toJSON b@Block{..} = object
        [ "hash"   .= blockHash (void b)
        , "header" .= blockHeader
        , "data"   .= blockData
        ]

instance FromJSON tx => FromJSON (Block tx ()) where
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

emptyGenesisBlock :: Timestamp -> s -> Block tx s
emptyGenesisBlock blockTimestamp blockState =
    mkBlock header []
  where
    header = BlockHeader
        { blockPrevHash = Crypto.toHashed Crypto.zeroHash
        , blockDataHash = Crypto.zeroHash
        , blockState
        , blockTimestamp
        , blockDifficulty = 0
        , blockNonce = 0
        }

blockHash :: Block tx s -> BlockHash
blockHash blk = headerHash (blockHeader blk)

linkBlock :: Monad m => Block tx s -> Block tx (s -> m t) -> m (Block tx t)
linkBlock (blockState . blockHeader -> s) = traverse ($ s)

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

prettyBlock :: (Crypto.Hashable tx, Pretty tx) => Block tx s -> Maybe Int -> Text
prettyBlock (Block bh@BlockHeader{..} txs) blockHeight = execWriter $ do
    tell $ formatHeaderWith (Fmt.stext % "━━ " % formatHashed % " ") height (headerHash bh)
    tell $ fencedLineFormat ("prevHash:   " % formatHashed) blockPrevHash
    tell $ fencedLineFormat ("timestamp:  " % Fmt.right 64 ' ') blockTimestamp
    tell $ "├" <> dashes 78 <> "┤\n"

    for_ (zip [0..Seq.length txs] (toList txs)) $ \(n, tx) -> do
        tell $ fencedLineFormat (Fmt.right 3 '0' % ":  " % Crypto.formatHashed) n (Crypto.hash tx)
        tell $ fencedLineFormat Fmt.stext ""
        let txContent = renderStrict $ layoutSmart layoutOptions $ pretty $ tx
        for_ (T.lines txContent) $ tell . fencedLineFormat Fmt.stext
        tell $ "├" <> dashes 78 <> "┤\n"

    tell $ "└" <> dashes 78 <> "┘\n"
  where
    formatHashed = Fmt.right 64 ' ' %. Crypto.formatHashed

    formatHeaderWith format = Fmt.sformat $ "┍━" % (Fmt.left  76 '━' %. format) % "━┑\n"
    fencedLineFormat format = Fmt.sformat $ "│ " % (Fmt.right 76 ' ' %. format) % " │\n"

    height = maybe "━━━━━━━" (\x -> " " <> show x <> " ") blockHeight

    dashes n = T.pack $ Prelude.replicate n '-'

    layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine 76 1.0}
