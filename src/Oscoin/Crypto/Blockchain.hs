module Oscoin.Crypto.Blockchain where

import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash
import           Oscoin.Prelude

import qualified Prelude

import           Data.Bifunctor (Bifunctor(..))
import           Data.Binary (Binary)
import qualified Data.ByteString.Char8 as C8
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           Data.Time.Clock (NominalDiffTime)
import           Text.Printf

newtype Blockchain tx s = Blockchain { fromBlockchain :: NonEmpty (Block tx s) }
    deriving (Functor, Traversable, Foldable)

instance (Hashable s, Binary tx) => Show (Blockchain tx s) where
    show = showBlockchain

instance Semigroup (Blockchain tx s) where
    (<>) (Blockchain a) (Blockchain b) = Blockchain (a <> b)

instance Bifunctor Blockchain where
    first f = Blockchain . fmap (first f) . fromBlockchain
    second f = Blockchain . fmap (second f) . fromBlockchain

fromList :: [Block tx s] -> Blockchain tx s
fromList = Blockchain . NonEmpty.fromList

infixr 5 |>

(|>) :: Block tx s -> Blockchain tx s -> Blockchain tx s
(|>) blk (Blockchain blks) = Blockchain (blk <| blks)

tip :: Blockchain tx s -> Block tx s
tip (Blockchain blks) = NonEmpty.head blks

height :: Blockchain tx s -> Int
height = length . fromBlockchain

validateBlockchain :: Hashable s => Blockchain tx s -> Either Error (Blockchain tx s)
validateBlockchain (Blockchain (blk :| [])) = do
    blk' <- validateBlock blk
    pure $ fromList [blk']
validateBlockchain (Blockchain (blk :| blk' : blks))
    | blockPrevHash (blockHeader blk) /= headerHash (blockHeader blk') =
        Left (Error "previous hash does not match")
    | t < t' =
        Left (Error "block timestamp is in the past")
    | t - t' > 2 * hours =
        Left (Error "block timestamp should be less than two hours in future")
    | otherwise =
        validateBlock blk *> validateBlockchain (Blockchain (blk' :| blks))
  where
    t  = blockTimestamp (blockHeader blk)
    t' = blockTimestamp (blockHeader blk')
    hours = 3600

blockHash :: Block tx s -> Hashed (BlockHeader ())
blockHash blk = headerHash (blockHeader blk)

showChainDigest :: Blockchain tx s -> String
showChainDigest =
    unwords . intersperse "←"
            . reverse
            . toList
            . map showBlockDigest
            . fromBlockchain

showBlockDigest :: Block tx s -> String
showBlockDigest b@Block{blockHeader} =
    printf "%s (%s)" (C8.unpack . shortHash . blockHash $ b) (show time)
  where
    time :: NominalDiffTime = toEnum (fromIntegral $ blockTimestamp blockHeader)

showBlockchain :: Binary tx => Blockchain tx s -> String
showBlockchain (Blockchain blks) = execWriter $ do
    tell "\n"
    for_ (zip heights (toList blks)) $ \(h, Block bh@BlockHeader{..} txs) -> do
        tell $ printf "┍━━━ %d ━━━ %s ━━━┑\n" (h :: Int) (C8.unpack $ toHex $ headerHash bh)
        tell $ printf "│ prevHash:   %-64s │\n" (C8.unpack $ toHex blockPrevHash)
        tell $ printf "│ timestamp:  %-64d │\n" blockTimestamp
        tell $ printf "│ rootHash:   %-64s │\n" (C8.unpack $ toHex blockStateHash)
        tell $ printf "├────────%s─────────┤\n" (Prelude.replicate 61 '─')

        for_ (zip [0..Seq.length txs] (toList txs)) $ \(n, tx) ->
            tell $ printf "│ %03d:  %-64s       │\n" n (C8.unpack $ toHex $ hashTx tx)

        tell $ printf "└────────%s─────────┘\n" (Prelude.replicate 61 '─')
  where
    heights = reverse [0..length blks - 1]
