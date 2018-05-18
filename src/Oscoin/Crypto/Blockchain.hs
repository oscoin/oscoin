module Oscoin.Crypto.Blockchain where

import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash
import           Oscoin.Prelude

import qualified Prelude

import qualified Data.ByteString.Char8 as C8
import           Data.Binary (Binary)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty ((<|))
import qualified Data.Sequence as Seq
import           Text.Printf
import           Data.Time.Clock (NominalDiffTime)

newtype Blockchain tx = Blockchain { fromBlockchain :: NonEmpty (Block tx) }
    deriving (Ord, Eq, Functor, Foldable, Traversable)

instance Binary tx => Show (Blockchain tx) where
    show = showBlockchain

fromList :: [Block tx] -> Blockchain tx
fromList = Blockchain . NonEmpty.fromList

infixr 5 |>

(|>) :: Block tx -> Blockchain tx -> Blockchain tx
(|>) blk (Blockchain blks) = Blockchain (blk <| blks)

tip :: Blockchain tx -> Block tx
tip (Blockchain blks) = NonEmpty.head blks

height :: Blockchain tx -> Int
height = length . fromBlockchain

validateBlockchain :: Blockchain tx -> Either Error (Blockchain tx)
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

blockHash :: Block tx -> Hashed BlockHeader
blockHash blk = headerHash (blockHeader blk)

headerHash :: BlockHeader -> Hashed BlockHeader
headerHash header =
    hash header

showChainDigest :: Blockchain tx -> String
showChainDigest =
    unwords . intersperse "←"
            . reverse
            . toList
            . map showBlockDigest
            . fromBlockchain

showBlockDigest :: Block tx -> String
showBlockDigest b@Block{blockHeader} =
    printf "%s (%s)" (C8.unpack . shortHash . blockHash $ b) (show time)
  where
    time :: NominalDiffTime = toEnum (fromIntegral $ blockTimestamp blockHeader)

showBlockchain :: Binary tx => Blockchain tx -> String
showBlockchain (Blockchain blks) = execWriter $ do
    tell "\n"
    for_ (zip heights (toList blks)) $ \(h, Block bh@BlockHeader{..} txs) -> do
        tell $ printf "┍━━━ %d ━━━ %s ━━━┑\n" (h :: Int) (C8.unpack $ toHex $ headerHash bh)
        tell $ printf "│ prevHash:   %-64s │\n" (C8.unpack $ toHex blockPrevHash)
        tell $ printf "│ timestamp:  %-64d │\n" blockTimestamp
        tell $ printf "│ rootHash:   %-64s │\n" (C8.unpack blockRootHash)
        tell $ printf "├────────%s─────────┤\n" (Prelude.replicate 61 '─')

        for_ (zip [0..Seq.length txs] (toList txs)) $ \(n, tx) ->
            tell $ printf "│ %03d:  %-64s       │\n" n (C8.unpack $ toHex $ hashTx tx)

        tell $ printf "└────────%s─────────┘\n" (Prelude.replicate 61 '─')
  where
    heights = reverse [0..length blks - 1]
