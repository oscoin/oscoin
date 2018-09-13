module Oscoin.Crypto.Blockchain
    ( Blockchain(..)
    , (|>)
    , tip
    , genesis
    , blocks
    , height
    , validateBlockchain
    , showBlockDigest
    , showChainDigest

    , module Oscoin.Crypto.Blockchain.Block
    ) where

import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash
import           Oscoin.Prelude hiding (toList)

import           Data.Bifunctor (Bifunctor(..))
import qualified Data.ByteString.Char8 as C8
import           Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Time.Clock (NominalDiffTime)
import           Text.Printf
import           GHC.Exts (IsList(toList))

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

tip :: Blockchain tx s -> Block tx s
tip (Blockchain blks) = NonEmpty.head blks

genesis :: Blockchain tx s -> Block tx s
genesis = NonEmpty.last . fromBlockchain

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
