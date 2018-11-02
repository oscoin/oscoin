module Oscoin.Test.Consensus.Class
    ( Msg (..)
    ) where

import           Oscoin.Prelude hiding (show)

import           Oscoin.Crypto.Blockchain (showBlockDigest)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)

import           Codec.Serialise (Serialise)
import           Formatting (formatToString, (%))
import qualified Formatting as F
import           Text.Show (Show(..))

data Msg tx s =
      BlockMsg    (Block tx s)
    | TxMsg       tx
    | ReqBlockMsg BlockHash
    deriving (Eq)

instance (Show tx, Serialise s) => Show (Msg tx s) where
    show (BlockMsg  blk) = formatToString (F.stext % " " % F.stext) "BlockMsg" (showBlockDigest blk)
    show (TxMsg     txs) = "TxMsg " ++ show txs
    show (ReqBlockMsg h) = "ReqBlockMsg " ++ show h
