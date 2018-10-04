module Oscoin.Crypto.Blockchain
    ( Blockchain(..)
    , ScoringFunction
    , TxLookup(..)
    , (|>)
    , tip
    , genesis
    , fromGenesis
    , blocks
    , takeBlocks
    , height
    , lookupTx
    , validateBlockchain
    , showBlockDigest
    , showChainDigest

    , module Oscoin.Crypto.Blockchain.Block
    ) where

import           Oscoin.Prelude hiding (toList)

import           Oscoin.Time
import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto

import           Control.Monad (guard)
import           Data.Bifunctor (Bifunctor(..))
import qualified Data.ByteString.Char8 as C8
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           Formatting ((%))
import qualified Formatting as F
import           GHC.Exts (IsList(..))
import           Numeric.Natural


newtype Blockchain tx s = Blockchain { fromBlockchain :: NonEmpty (Block tx s) }
    deriving (Show, Functor, Traversable, Foldable)

instance Semigroup (Blockchain tx s) where
    (<>) (Blockchain a) (Blockchain b) = Blockchain (a <> b)

instance Bifunctor Blockchain where
    first f = Blockchain . map (first f) . fromBlockchain
    second f = Blockchain . map (second f) . fromBlockchain

instance IsList (Blockchain tx s) where
    type Item (Blockchain tx s) = Block tx s

    fromList = Blockchain . NonEmpty.fromList
    toList   = NonEmpty.toList . fromBlockchain

infixr 5 |>

(|>) :: Block tx s -> Blockchain tx s -> Blockchain tx s
(|>) blk (Blockchain blks) = Blockchain (blk <| blks)

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

height :: Blockchain tx s -> Int
height = length . fromBlockchain

-- | Scoring function for blockchains.
type ScoringFunction tx s = Blockchain tx s -> Blockchain tx s -> Ordering

data TxLookup tx = TxLookup
    { txPayload       :: tx
    , txBlockHash     :: BlockHash
    , txConfirmations :: Natural
    }

lookupTx :: forall tx s. Crypto.Hashable tx => Crypto.Hashed tx -> Blockchain tx s -> Maybe (TxLookup tx)
lookupTx h (blocks -> chain) = listToMaybe $ do
    (i, block) <- zip [1..] chain
    tx <- toList $ blockData block
    guard (Crypto.hash tx == h)
    pure $ TxLookup tx (headerHash $ blockHeader block) i

validateBlockchain :: Crypto.Hashable s => Blockchain tx s -> Either Text (Blockchain tx s)
validateBlockchain (Blockchain (blk :| [])) = do
    blk' <- validateBlock blk
    pure $ fromList [blk']
validateBlockchain (Blockchain (blk :| blk' : blks))
    | blockPrevHash (blockHeader blk) /= headerHash (blockHeader blk') =
        Left "previous hash does not match"
    | t < t' =
        Left "block timestamp is in the past"
    | t - t' > 2 * hours =
        Left "block timestamp should be less than two hours in future"
    | otherwise =
        validateBlock blk *> validateBlockchain (Blockchain (blk' :| blks))
  where
    t  = ts blk
    t' = ts blk'
    ts = sinceEpoch . blockTimestamp . blockHeader

showChainDigest :: Blockchain tx s -> Text
showChainDigest =
    T.unwords . intersperse "←"
              . reverse
              . toList
              . map showBlockDigest
              . fromBlockchain

showBlockDigest :: Block tx s -> Text
showBlockDigest b@Block{blockHeader} =
    F.sformat (F.string % "(" % F.build % ")")
              (C8.unpack . Crypto.shortHash . blockHash $ b)
              (blockTimestamp blockHeader)
