module Oscoin.Crypto.Blockchain
    ( Blockchain(..)
    , ScoringFunction
    , TxLookup(..)
    , (|>)
    , unsafeToBlockchain
    , tip
    , genesis
    , fromGenesis
    , blocks
    , takeBlocks
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

import           Control.Monad (guard)
import qualified Data.ByteString.Char8 as C8
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           Formatting ((%))
import qualified Formatting as F
import           GHC.Exts (IsList(..))
import           Numeric.Natural


newtype Blockchain tx s = Blockchain { fromBlockchain :: NonEmpty (Block tx s) }
    deriving (Show)

instance Semigroup (Blockchain tx s) where
    (<>) (Blockchain a) (Blockchain b) = Blockchain (a <> b)

instance IsList (Blockchain tx s) where
    type Item (Blockchain tx s) = Block tx s

    fromList = Blockchain . NonEmpty.fromList
    toList   = NonEmpty.toList . fromBlockchain

infixr 5 |>

(|>) :: Block tx s -> Blockchain tx s -> Blockchain tx s
(|>) blk (Blockchain blks) = Blockchain (blk <| blks)

unsafeToBlockchain :: [Block tx s] -> Blockchain tx s
unsafeToBlockchain blks =
    Blockchain $ NonEmpty.fromList blks

blocks :: Blockchain tx s -> [Block tx s]
blocks = NonEmpty.toList . fromBlockchain

takeBlocks :: Int -> Blockchain tx s -> [Block tx s]
takeBlocks n = NonEmpty.take n . fromBlockchain

tip :: Blockchain tx s -> Block tx s
tip (Blockchain blks) = NonEmpty.head blks

genesis :: Blockchain tx s -> Block tx s
genesis = NonEmpty.last . fromBlockchain

fromGenesis :: Block tx s -> Blockchain tx s
fromGenesis g = Blockchain (g :| [])

-- | Returns the height of the chain. That is, the number of blocks not including
-- the genesis. A 'Blockchain' with only a genesis block will thus have a height
-- of zero.
height :: Blockchain tx s -> Height
height chain = fromIntegral $ chainLength chain - 1

-- | Returns the length of a chain, or total number of blocks including the genesis.
chainLength :: Blockchain tx s -> Int
chainLength = NonEmpty.length . fromBlockchain

-- | Scoring function for blockchains.
type ScoringFunction tx s = Blockchain tx s -> Blockchain tx s -> Ordering

data TxLookup tx = TxLookup
    { txPayload       :: tx
    , txBlockHash     :: BlockHash
    , txConfirmations :: Natural
    }

lookupTx :: forall tx s. (Crypto.Hashable tx) => Crypto.Hashed tx -> Blockchain tx s -> Maybe (TxLookup tx)
lookupTx h (blocks -> chain) = listToMaybe $ do
    (i, block) <- zip [1..] chain
    tx <- toList $ blockData block
    guard (Crypto.hash tx == h)
    pure $ TxLookup tx (blockHash block) i

showChainDigest :: Blockchain tx s -> Text
showChainDigest =
    T.unwords . intersperse "←"
              . reverse
              . toList
              . map showBlockDigest
              . fromBlockchain

showBlockDigest :: Block tx s -> Text
showBlockDigest b@Block{blockHeader} =
    F.sformat ("[" % F.string % " @" % F.build % "]")
              (C8.unpack . Crypto.shortHash . blockHash $ b)
              (prettyDuration $ sinceEpoch (blockTimestamp blockHeader))
