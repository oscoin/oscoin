{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Crypto.Blockchain
    ( Blockchain(..)
    , TxLookup(..)
    , (|>)
    , unsafeToBlockchain
    , tip
    , genesis
    , fromGenesis
    , blocks
    , blocks'
    , takeBlocks
    , takeWhileBlocks
    , height
    , chainLength
    , lookupTx
    , showBlockDigest
    , showChainDigest

    , module Oscoin.Crypto.Blockchain.Block
    ) where

import           Oscoin.Prelude hiding (toList)

import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Time
import           Oscoin.Time.Chrono (NewestFirst(..), toNewestFirst)

import           Control.Monad (guard)
import qualified Data.ByteString.Char8 as C8
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           Formatting ((%))
import qualified Formatting as F
import           GHC.Exts (IsList(..))
import           Numeric.Natural

-- | A 'Blockchain' is a non-empty /sealed/ list of blocks, with the /most recent
-- block in front/.
newtype Blockchain c tx s =
    Blockchain { fromBlockchain :: NonEmpty (Block c tx (Sealed c s)) }
    deriving (Generic)

deriving instance (Show (Crypto.Hash c), Show tx, Show s) => Show (Blockchain c tx s)

instance Semigroup (Blockchain c tx s) where
    (<>) (Blockchain a) (Blockchain b) = Blockchain (a <> b)

instance IsList (Blockchain c tx s) where
    type Item (Blockchain c tx s) = Block c tx (Sealed c s)

    fromList = Blockchain . NonEmpty.fromList
    toList   = NonEmpty.toList . fromBlockchain

infixr 5 |>

-- | Appends a new 'Block' in front of the input 'Blockchain', making it
-- /the new tip/.
(|>) :: Block c tx (Sealed c s) -> Blockchain c tx s -> Blockchain c tx s
(|>) blk (Blockchain blks) = Blockchain (blk <| blks)

unsafeToBlockchain :: [Block c tx (Sealed c s)] -> Blockchain c tx s
unsafeToBlockchain blks =
    Blockchain $ NonEmpty.fromList blks

blocks :: Blockchain c tx s -> NewestFirst [] (Block c tx (Sealed c s))
blocks = NewestFirst . NonEmpty.toList . fromBlockchain

blocks' :: Blockchain c tx s -> NewestFirst NonEmpty (Block c tx (Sealed c s))
blocks' = NewestFirst . fromBlockchain

takeBlocks :: Int -> Blockchain c tx s -> [Block c tx (Sealed c s)]
takeBlocks n = NonEmpty.take n . fromBlockchain

takeWhileBlocks :: (Block c tx (Sealed c s) -> Bool)
                -> Blockchain c tx s
                -> [Block c tx (Sealed c s)]
takeWhileBlocks p = NonEmpty.takeWhile p . fromBlockchain

tip :: Blockchain c tx s -> Block c tx (Sealed c s)
tip (Blockchain blks) = NonEmpty.head blks

genesis :: Blockchain c tx s -> Block c tx (Sealed c s)
genesis = NonEmpty.last . fromBlockchain

fromGenesis :: Block c tx (Sealed c s) -> Blockchain c tx s
fromGenesis g = Blockchain (g :| [])

-- | Returns the height of the chain. That is, the number of blocks not including
-- the genesis. A 'Blockchain' with only a genesis block will thus have a height
-- of zero.
height :: Blockchain c tx s -> Height
height chain = fromIntegral $ chainLength chain - 1

-- | Returns the length of a chain, or total number of blocks including the genesis.
chainLength :: Blockchain c tx s -> Int
chainLength = NonEmpty.length . fromBlockchain

data TxLookup c tx = TxLookup
    { txPayload       :: tx
    , txBlockHash     :: BlockHash c
    , txConfirmations :: Natural
    }

lookupTx
    :: forall c tx s. (Crypto.Hashable c tx)
    => Crypto.Hashed c tx
    -> Blockchain c tx s
    -> Maybe (TxLookup c tx)
lookupTx h (toNewestFirst . blocks -> chain) = listToMaybe $ do
    (i, block) <- zip [1..] chain
    tx <- toList $ blockData block
    guard (Crypto.hash tx == h)
    pure $ TxLookup tx (blockHash block) i

showChainDigest :: Crypto.HasHashing c => Blockchain c tx s -> Text
showChainDigest =
    T.unwords . intersperse "←"
              . reverse
              . toList
              . map showBlockDigest
              . fromBlockchain

showBlockDigest :: Crypto.HasHashing c => Block c tx s -> Text
showBlockDigest b =
    F.sformat ("[" % F.string % " (p:" % F.string % ") @" % F.build % "]")
              (C8.unpack . Crypto.compactHash . blockHash $ b)
              (C8.unpack . Crypto.compactHash . blockPrevHash . blockHeader $ b)
              (prettyDuration $ sinceEpoch (blockTimestamp . blockHeader $ b))
