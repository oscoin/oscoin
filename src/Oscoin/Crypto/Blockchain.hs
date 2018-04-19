module Oscoin.Crypto.Blockchain where

import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash
import           Oscoin.Prelude

import qualified Prelude

import qualified Data.ByteString.Char8 as C8
import           Data.Binary (Binary)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           Text.Printf

-- TODO: Use this representation for Blockchain.
--
-- data Blockchain
--     = Genesis    (Block tx)
--     | Blockchain (Block tx) Blockchain

type Blockchain tx = NonEmpty (Block tx)

validateBlockchain :: Blockchain tx -> Either Error (Blockchain tx)
validateBlockchain (blk :| []) = do
    blk' <- validateBlock blk
    pure $ NonEmpty.fromList [blk']
validateBlockchain (blk :| blk' : blks)
    | blockPrevHash (blockHeader blk) /= headerHash (blockHeader blk') =
        Left (Error "previous hash does not match")
    | t < t' =
        Left (Error "block timestamp is in the past")
    | t - t' > 2 * hours =
        Left (Error "block timestamp should be less than two hours in future")
    | otherwise =
        validateBlock blk *> validateBlockchain (blk' :| blks)
  where
    t  = blockTimestamp (blockHeader blk)
    t' = blockTimestamp (blockHeader blk')
    hours = 3600

blockHash :: Block tx -> Hashed BlockHeader
blockHash blk = headerHash (blockHeader blk)

headerHash :: BlockHeader -> Hashed BlockHeader
headerHash header =
    hash header

-- TODO: Don't use Prelude.replicate.
printBlockchain :: Binary tx => Blockchain tx -> IO ()
printBlockchain blks = do
    printf "\n"
    for_ (zip heights (toList blks)) $ \(h, Block bh@BlockHeader{..} txs) -> do
        printf "┍━━━ %d ━━━ %s ━━━┑\n" (h :: Int) (C8.unpack $ toHex $ headerHash bh)
        printf "│ prevHash:   %-64s │\n" (C8.unpack $ toHex blockPrevHash)
        printf "│ timestamp:  %-64d │\n" blockTimestamp
        printf "│ rootHash:   %-64s │\n" (C8.unpack blockRootHash)
        printf "├────────%s─────────┤\n" (Prelude.replicate 61 '─')

        for_ (zip [0..Seq.length txs] (toList txs)) $ \(n, tx) ->
            printf "│ %03d:  %-64s       │\n" n (C8.unpack $ toHex $ hashTx tx)

        printf "└────────%s─────────┘\n" (Prelude.replicate 61 '─')
  where
    heights = reverse [0..NonEmpty.length blks - 1]
