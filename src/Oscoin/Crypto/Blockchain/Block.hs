module Oscoin.Crypto.Blockchain.Block where

import           Oscoin.Prelude
import           Oscoin.Crypto.Hash

import           Data.Binary (Binary, encode)
import           Crypto.Hash (hashlazy)
import qualified Crypto.Hash.MerkleTree as Merkle
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteArray (zero)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

-- | Block height.
type Height = Integer

-- | Block header.
data BlockHeader = BlockHeader
    { blockPrevHash   :: Hashed BlockHeader
    , blockRootHash   :: ByteString -- TODO: Should be Digest.
    , blockTimestamp  :: Timestamp
    } deriving (Show, Eq, Ord, Generic)

instance Binary BlockHeader
instance Hashable BlockHeader

-- | Create an empty block header.
emptyHeader :: BlockHeader
emptyHeader = BlockHeader
    { blockPrevHash = toHashed zeroHash
    , blockRootHash = zero 32
    , blockTimestamp = 0
    }

-- | Block. @tx@ is the type of transaction stored in this block.
data Block tx = Block
    { blockHeader :: BlockHeader
    , blockData   :: Seq tx
    } deriving (Show, Generic, Functor, Foldable, Traversable)

instance (Binary tx) => Binary (Block tx)
deriving instance Eq tx => Eq (Block tx)
deriving instance Ord tx => Ord (Block tx)

validateBlock :: Block tx -> Either Error (Block tx)
validateBlock = Right

block
    :: (Foldable t, Binary tx)
    => Hashed BlockHeader
    -> Timestamp
    -> t tx
    -> Block tx
block prev t txs =
    Block
        BlockHeader
            { blockPrevHash  = prev
            , blockTimestamp = t
            , blockRootHash  = hashTxs txs
            }
        (Seq.fromList (toList txs))

genesisBlock :: (Foldable t, Binary tx) => Timestamp -> t tx -> Block tx
genesisBlock t xs =
    block (toHashed zeroHash) t xs

isGenesisBlock :: Block a -> Bool
isGenesisBlock blk =
    (blockPrevHash . blockHeader) blk == toHashed zeroHash

hashTxs :: (Foldable t, Binary tx) => t tx -> ByteString
hashTxs txs
    -- TODO: Get rid of merkle-tree dependency, or create our own that doesn't
    -- depend on protolude.
    -- TODO: Needs to return `Hashed (t tx)` or something.
    | null txs = zero 32
    | otherwise =
          Merkle.mtHash
        . Merkle.mkMerkleTree
        $ map (toStrict . encode) (toList txs)

hashTx :: Binary tx => tx -> Hashed tx
hashTx tx =
    toHashed (hashlazy (encode tx))
