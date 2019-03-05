{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE UndecidableInstances      #-}
module Oscoin.Crypto.Blockchain.Block
    ( -- * Types
      Block -- opaque to disallow construction of sealed blocks.
    , BlockHash
    , BlockHeader(..)
    , Unsealed
    , Sealed(..)
    , StateHash
    , Height
    , Depth
    , Score
    , Timestamp

    -- * Smart constructors and data getters
    , mkBlock
    , blockHeader
    , blockHash
    , blockData
    , emptyGenesisBlock
    , emptyGenesisFromState
    , genesisBlock
    , isGenesisBlock
    , headerHash
    , parentHash
    , withHeader
    , blockScore
    , sealBlock
    , linkParent
    , emptyHeader
    , hashState
    , hashTxs

    , module Oscoin.Crypto.Blockchain.Block.Difficulty
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block.Difficulty
import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Time

import           Codec.Serialise (Serialise(..))
import qualified Codec.Serialise as Serialise
import qualified Codec.Serialise.Decoding as Serialise
import qualified Codec.Serialise.Encoding as Serialise
import           Control.Monad (fail)
import qualified Crypto.Data.Auth.Tree as AuthTree
import qualified Crypto.Data.Auth.Tree.Internal as AuthTree
import           Data.Aeson
                 (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           Numeric.Natural
import           Text.Show (Show(..))

-- | Block height.
type Height = Integer

-- | Block depth.
type Depth = Natural

-- | Block score.
type Score = Integer

-- | This apparently-useless type allows us to divide the block world into
-- two categories: unsealed vs sealed blocks. This is very useful as it creates
-- a very distinct boundary between the two, with 'sealBlock' being the only
-- function capable of turning an unsealed block into a sealed one. This usually
-- happens during the mining.
data Unsealed = Unsealed deriving (Generic, Show)

-- NOTE(adn) In the midst of the refactoring for oscoin#368, at some point
-- GHC started requesting me a 'Serialise Unsealed' constraint in various
-- places. My intuition is that /in theory/ we should never serialise an
-- 'Unsealed' block, so we should do another sweep through the different
-- constraints and try to pinpoint why is that. For now, the path of least
-- resistence is to simply provide such (dubious) instance.
instance Serialise Unsealed

newtype Sealed c s = SealedWith s
    deriving (Eq, Generic, Show, Serialise)

-- | Block header.
data BlockHeader crypto s = BlockHeader
    { blockPrevHash         :: Crypto.Hash crypto
    , blockDataHash         :: Crypto.Hash crypto
    , blockStateHash        :: Crypto.Hash crypto
    , blockTimestamp        :: Timestamp
    , blockTargetDifficulty :: Difficulty
    , blockSeal             :: s
    } deriving Generic

deriving instance (Show (Hash c), Show s) => Show (BlockHeader c s)
deriving instance (Ord (Hash c), Ord s) => Ord (BlockHeader c s)
deriving instance (Eq (Hash c), Eq s) => Eq (BlockHeader c s)

instance (Serialise (Hash c), Serialise s) => Serialise (BlockHeader c s)

instance (Serialise (Hash c), Serialise s, Crypto.HasHashing c)
    => Crypto.Hashable c (BlockHeader c s) where
    hash = Crypto.hashSerial

instance ToJSON s => ToJSON (Sealed c s) where
    toJSON (SealedWith s) = toJSON s

instance FromJSON s => FromJSON (Sealed c s) where
    parseJSON x = SealedWith <$> parseJSON x

instance (ToJSON (Hash c), ToJSON s) => ToJSON (BlockHeader c s) where
    toJSON BlockHeader{..} = object
        [ "parentHash"       .= blockPrevHash
        , "timestamp"        .= blockTimestamp
        , "dataHash"         .= blockDataHash
        , "stateHash"        .= blockStateHash
        , "seal"             .= blockSeal
        , "targetDifficulty" .= blockTargetDifficulty
        ]

instance (FromJSON (Hash c), FromJSON s) => FromJSON (BlockHeader c s) where
  parseJSON = withObject "BlockHeader" $ \o -> do
        blockPrevHash         <- o .: "parentHash"
        blockTimestamp        <- o .: "timestamp"
        blockDataHash         <- o .: "dataHash"
        blockStateHash        <- o .: "stateHash"
        blockSeal             <- o .: "seal"
        blockTargetDifficulty <- o .: "targetDifficulty"

        pure BlockHeader{..}

-- | Create an empty block header.
emptyHeader :: Crypto.HasHashing c => BlockHeader c Unsealed
emptyHeader = BlockHeader
    { blockPrevHash = Crypto.zeroHash
    , blockDataHash = Crypto.zeroHash
    , blockStateHash = Crypto.zeroHash
    , blockSeal = Unsealed
    , blockTimestamp = epoch
    , blockTargetDifficulty = unsafeDifficulty 0
    }


-- NOTE(adn) Turn this into a typeclass?
type HasBlockHeader c s =
    ( Crypto.Hashable c (BlockHeader c s)
    , Crypto.HasHashing c
    )

headerHash
    :: (HasBlockHeader c s)
    => BlockHeader c s
    -> BlockHash c
headerHash =
    Crypto.fromHashed . Crypto.hash

parentHash :: Block c tx s -> BlockHash c
parentHash = blockPrevHash . blockHeader

withHeader
    :: (HasBlockHeader c s)
    => Block c tx a
    -> BlockHeader c s
    -> Block c tx s
withHeader blk h = blk { blockHeader = h, blockHash = headerHash h }

-- | Set the block parent hash of a block to the supplied parent.
linkParent
    :: (HasBlockHeader c s)
    => Block c tx s -- ^ The parent block
    -> Block c tx s -- ^ The unlinked child block
    -> Block c tx s -- ^ The newly-linked child
linkParent p blk =
    blk { blockHeader = header, blockHash = headerHash header }
  where
    header = (blockHeader blk) { blockPrevHash = blockHash p }


-- | The hash of a block.
type BlockHash crypto = Crypto.Hash crypto

-- | The hash of a state tree.
type StateHash crypto = Crypto.Hash crypto

-- | Block. @tx@ is the type of transaction stored in this block.
--
-- Nb. There is no instance for 'Functor' on 'Block' because updating the @s@
-- parameter would require the side-effect of updating the 'BlockHash' for
-- the update to be valid. Instead, use the 'sealBlock' function.
data Block c tx s = Block
    { blockHeader :: BlockHeader c s
    , blockHash   :: BlockHash c
    , blockData   :: Seq tx
    } deriving Generic

deriving instance (Show (BlockHeader c s), Show (BlockHash c), Show tx) => Show (Block c tx s)
deriving instance (Eq (Hash c), Eq tx, Eq s) => Eq (Block c tx s)
deriving instance (Ord (Hash c), Ord tx, Ord s) => Ord (Block c tx s)

instance ( Serialise tx
         , Serialise s
         , HasBlockHeader c s
         , Serialise (Hash c)
         , Eq (Hash c)
         ) => Serialise (Block c tx s) where
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

instance (ToJSON s, ToJSON (Hash c), ToJSON tx) => ToJSON (Block c tx s) where
    toJSON Block{..} = object
        [ "hash"   .= blockHash
        , "header" .= blockHeader
        , "data"   .= blockData
        ]

instance ( FromJSON s
         , FromJSON (Hash c)
         , FromJSON tx
         , HasBlockHeader c s
         , Eq (Hash c)
         ) => FromJSON (Block c tx s) where
  parseJSON = withObject "Block" $ \o -> do
        blockHeader <- o .: "header"
        blockData   <- o .: "data"
        blockHash   <- o .: "hash"

        if headerHash blockHeader /= blockHash
           then fail "Error decoding block: hash does not match data"
           else pure Block{..}

mkBlock
    :: ( Foldable t
       , HasBlockHeader c s
       )
    => BlockHeader c s
    -> t tx
    -> Block c tx s
mkBlock header txs =
    Block header (headerHash header) (Seq.fromList (toList txs))

emptyGenesisBlock
    :: (HasBlockHeader c Unsealed)
    => Timestamp
    -> Block c tx Unsealed
emptyGenesisBlock blockTimestamp =
    mkBlock header []
  where
    header = emptyHeader { blockTimestamp }

emptyGenesisFromState
    :: ( HasBlockHeader c Unsealed
       , Crypto.Hashable c st
       )
    => Timestamp
    -> st
    -> Block c tx Unsealed
emptyGenesisFromState blockTimestamp st =
    mkBlock header []
  where
    header = emptyHeader { blockTimestamp, blockStateHash = stHash }
    stHash = Crypto.fromHashed . Crypto.hash $ st

-- | Construct a sealed genesis block.
genesisBlock
    :: (Serialise tx
      , HasBlockHeader c s
      , AuthTree.MerkleHash (Hash c)
      , Crypto.Hashable c (BlockHeader c Unsealed)
      , Crypto.Hashable c (BlockHeader c (Sealed c s))
      , Crypto.Hashable c st)
    => [tx]
    -> st
    -> s
    -> Timestamp
    -> Block c tx (Sealed c s)
genesisBlock txs st seal t =
    sealBlock seal $ mkBlock header txs
  where
    header = emptyHeader
        { blockTimestamp = t
        , blockStateHash = hashState st
        , blockDataHash = hashTxs txs
        }

isGenesisBlock
    :: ( Crypto.HasHashing c
       , Eq (Hash c)
       )
    => Block c tx s
    -> Bool
isGenesisBlock Block{..} =
    blockPrevHash blockHeader == Crypto.zeroHash


sealBlock
    :: ( Crypto.Hashable c (BlockHeader c (Sealed c s)) )
    => s
    -> Block c tx Unsealed
    -> Block c tx (Sealed c s)
sealBlock seal blk =
    blk' { blockHash = headerHash (blockHeader blk') }
  where
    blk' = blk { blockHeader =
        (blockHeader blk) { blockSeal = SealedWith seal } }


blockScore :: Block crypto tx s -> Integer
blockScore = fst . decodeDifficulty . blockTargetDifficulty . blockHeader

hashState :: Crypto.Hashable c st => st -> StateHash c
hashState = Crypto.fromHashed . Crypto.hash

hashTxs
    :: ( Foldable t
       , Serialise tx
       , Crypto.HasHashing c
       , AuthTree.MerkleHash (Crypto.Hash c)
       )
    => t tx
    -> Crypto.Hash c
hashTxs (toList -> txs)
    | null txs = Crypto.zeroHash
    | otherwise = AuthTree.merkleHash
                . AuthTree.fromList
                $ [(tx, mempty :: ByteString) | tx <-
                    map (LBS.toStrict . Serialise.serialise) txs]
                -- Nb. Since our Merkle tree works with key-value pairs, but we're only
                -- really interested in the keys being present or absent for this use-case,
                -- we use the empty byte string as the value component.

