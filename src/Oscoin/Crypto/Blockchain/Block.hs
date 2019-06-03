{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE UndecidableInstances      #-}
module Oscoin.Crypto.Blockchain.Block
    ( -- * Types
      Block -- opaque to disallow construction of sealed blocks.
    , SealedBlock
    , BlockHash
    , BlockHeader(..)
    , BlockData(..)
    , Unsealed(..)
    , Sealed(..)
    , StateHash
    , Height
    , Depth
    , Score
    , ScoreFn
    , Timestamp
    , Beneficiary

    -- * Smart constructors and data getters
    , mkBlock
    , mkBlockData
    , blockHeader
    , blockHash
    , blockData
    , blockTxs
    , blockBeneficiary
    , emptyGenesisBlock
    , emptyGenesisFromState
    , isGenesisBlock
    , headerHash
    , parentHash
    , sealBlock
    , linkParent
    , emptyHeader
    , hashState
    , hashData

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
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           Lens.Micro
import           Numeric.Natural
import           Text.Show (Show(..))

-- | Block height.
type Height = Integer

-- | Block depth.
type Depth = Natural

-- | Block score.
type Score = Integer

-- | A function that scores blocks.
type ScoreFn tx c s = Block tx c s -> Score

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
    deriving (Eq, Ord, Generic, Show, Serialise)

-- | Block header.
data BlockHeader crypto s = BlockHeader
    { blockHeight           :: Height
    , blockPrevHash         :: Crypto.Hash crypto
    , blockDataHash         :: Crypto.Hash crypto
    , blockStateHash        :: Crypto.Hash crypto
    , blockTimestamp        :: Timestamp
    , blockTargetDifficulty :: Difficulty
    , blockSeal             :: s
    } deriving Generic

deriving instance (Show (Hash c), Show s) => Show (BlockHeader c s)
deriving instance (Ord (Hash c), Ord s) => Ord (BlockHeader c s)
deriving instance (Eq (Hash c), Eq s) => Eq (BlockHeader c s)

instance (Serialise (Hash c), Serialise s) => Serialise (BlockHeader c s) where
    encode BlockHeader{..} =
           Serialise.encodeListLen 8
        <> Serialise.encodeWord 0
        <> Serialise.encode blockHeight
        <> Serialise.encode blockPrevHash
        <> Serialise.encode blockDataHash
        <> Serialise.encode blockStateHash
        <> Serialise.encode blockTimestamp
        <> Serialise.encode blockTargetDifficulty
        <> Serialise.encode blockSeal

    decode = do
        Serialise.decodeListLenOf 8
        tag <- Serialise.decodeWord
        case tag of
            0 -> do
                !blockHeight           <- Serialise.decode
                !blockPrevHash         <- Serialise.decode
                !blockDataHash         <- Serialise.decode
                !blockStateHash        <- Serialise.decode
                !blockTimestamp        <- Serialise.decode
                !blockTargetDifficulty <- Serialise.decode
                !blockSeal             <- Serialise.decode

                pure BlockHeader{..}
            _ ->
                fail "Error decoding block header: unknown tag"


instance (Serialise (Hash c), Serialise s, Crypto.HasHashing c)
    => Crypto.Hashable c (BlockHeader c s) where
    hash = Crypto.hashSerial

blockHeaderSealL :: Lens (BlockHeader c s) (BlockHeader c s') s s'
blockHeaderSealL = lens blockSeal (\h x -> h { blockSeal = x })

blockHeaderPrevHashL :: Lens' (BlockHeader c s) (BlockHash c)
blockHeaderPrevHashL = lens blockPrevHash (\h x -> h { blockPrevHash = x })

-- | Create an empty block header.
emptyHeader :: Crypto.HasHashing c => BlockHeader c Unsealed
emptyHeader = BlockHeader
    { blockHeight   = 0
    , blockPrevHash = Crypto.zeroHash
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

-- | Set the block parent hash of a block to the supplied parent.
linkParent
    :: (HasBlockHeader c s)
    => Block c tx s -- ^ The parent block
    -> Block c tx s -- ^ The unlinked child block
    -> Block c tx s -- ^ The newly-linked child
linkParent parent blk =
    blk & blockHeaderL . blockHeaderPrevHashL .~ blockHash parent


-- | The hash of a block.
type BlockHash crypto = Crypto.Hash crypto

-- | The hash of a state tree.
type StateHash crypto = Crypto.Hash crypto

-- | The beneficiary of the block reward.
type Beneficiary crypto = Crypto.ShortHash crypto

-- | Block. @tx@ is the type of transaction stored in this block.
--
-- Nb. There is no instance for 'Functor' on 'Block' because updating the @s@
-- parameter would require the side-effect of updating the 'BlockHash' for
-- the update to be valid. Instead, use the 'sealBlock' function.
data Block c tx s = Block
    { blockHeader :: BlockHeader c s
    , blockHash   :: BlockHash c
    , blockData   :: BlockData c tx
    } deriving Generic

type SealedBlock c tx s = Block c tx (Sealed c s)

data BlockData c tx = BlockData
    { blockDataBeneficiary :: Beneficiary c
    , blockDataTxs         :: Seq tx
    } deriving Generic

blockTxs :: Block c tx s -> Seq tx
blockTxs = blockDataTxs . blockData

blockBeneficiary :: Block c tx s -> Beneficiary c
blockBeneficiary = blockDataBeneficiary . blockData

instance (Serialise tx, Serialise (Beneficiary c)) => Serialise (BlockData c tx) where
    encode BlockData{..} =
           Serialise.encodeListLen 2
        <> Serialise.encodeWord 0
        <> Serialise.encode blockDataBeneficiary
        <> Serialise.encode blockDataTxs

    decode = do
        Serialise.decodeListLenOf 2
        tag <- Serialise.decodeWord
        case tag of
            0 -> do
                !blockDataBeneficiary <- Serialise.decode
                !blockDataTxs         <- Serialise.decode

                pure BlockData{..}
            _ ->
                fail "Error decoding block data: unknown tag"

deriving instance (Show (Beneficiary c), Show s) => Show (BlockData c s)
deriving instance (Ord (Beneficiary c), Ord s) => Ord (BlockData c s)
deriving instance (Eq (Beneficiary c), Eq s) => Eq (BlockData c s)

deriving instance (Show (BlockData c tx), Show (BlockHeader c s), Show (BlockHash c), Show tx)
    => Show (Block c tx s)
deriving instance (Eq (BlockData c tx), Eq (Hash c), Eq tx, Eq s) => Eq (Block c tx s)
deriving instance (Ord (BlockData c tx), Ord (Hash c), Ord tx, Ord s) => Ord (Block c tx s)

instance ( Serialise tx
         , Serialise s
         , Serialise (BlockData c tx)
         , HasBlockHeader c s
         , Serialise (Hash c)
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


blockHeaderL
    :: (HasBlockHeader c s')
    => Lens (Block c tx s) (Block c tx s') (BlockHeader c s) (BlockHeader c s')
blockHeaderL = lens blockHeader (\b h -> mkBlock h (blockBeneficiary b) (blockTxs b))

mkBlock
    :: ( Foldable t
       , HasBlockHeader c s
       )
    => BlockHeader c s
    -> Beneficiary c
    -> t tx
    -> Block c tx s
mkBlock header benef txs =
    Block header (headerHash header) (mkBlockData benef txs)

mkBlockData :: Foldable t => Beneficiary c -> t tx -> BlockData c tx
mkBlockData benef txs =
    BlockData
        { blockDataBeneficiary = benef
        , blockDataTxs         = Seq.fromList (toList txs)
        }

emptyGenesisBlock
    :: (HasBlockHeader c Unsealed)
    => Timestamp
    -> Beneficiary c
    -> Block c tx Unsealed
emptyGenesisBlock blockTimestamp benef =
    mkBlock header benef []
  where
    header = emptyHeader { blockTimestamp }

emptyGenesisFromState
    :: ( HasBlockHeader c Unsealed
       , Crypto.Hashable c st
       )
    => Timestamp
    -> Beneficiary c
    -> st
    -> Block c tx Unsealed
emptyGenesisFromState blockTimestamp benef st =
    mkBlock header benef []
  where
    header = emptyHeader { blockTimestamp, blockStateHash = stHash }
    stHash = Crypto.fromHashed . Crypto.hash $ st

isGenesisBlock
    :: (Crypto.HasHashing c)
    => Block c tx s
    -> Bool
isGenesisBlock Block{..} =
    blockPrevHash blockHeader == Crypto.zeroHash


sealBlock
    :: Crypto.Hashable c (BlockHeader c (Sealed c s))
    => s
    -> Block c tx Unsealed
    -> Block c tx (Sealed c s)
sealBlock seal blk =
    blk & (blockHeaderL . blockHeaderSealL) .~ SealedWith seal


hashState :: Crypto.Hashable c st => st -> StateHash c
hashState = Crypto.fromHashed . Crypto.hash

hashData
    :: ( Serialise tx
       , Serialise (Beneficiary c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       )
    => BlockData c tx -> Crypto.Hash c
hashData BlockData{..} =
    AuthTree.merkleHash . AuthTree.fromList $ bs
  where
    bs :: [(ByteString, ByteString)]
    bs = map (first LBS.toStrict)
       $ (Serialise.serialise blockDataBeneficiary, mempty)
       : [(Serialise.serialise tx, mempty) | tx <- toList blockDataTxs]
    -- Nb. Since our Merkle tree works with key-value pairs, but we're only
    -- really interested in the keys being present or absent for this use-case,
    -- we use the empty byte string as the value component.
