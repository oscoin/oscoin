module Oscoin.Test.Consensus.Class
    ( Msg (..)
    , module Oscoin.Clock
    ) where

import           Oscoin.Prelude hiding (show)

import           Oscoin.Clock (MonadClock(..), Tick)
import           Oscoin.Crypto.Blockchain (showBlockDigest)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)

import           Formatting (formatToString, (%))
import qualified Formatting as F
import           Text.Show (Show(..))

data Msg tx =
      BlockMsg    (Block tx ())
    | TxMsg       tx
    | ReqBlockMsg BlockHash
    deriving (Eq)

instance Show tx => Show (Msg tx) where
    show (BlockMsg  blk) = formatToString (F.stext % " " % F.stext) "BlockMsg" (showBlockDigest blk)
    show (TxMsg     txs) = "TxMsg " ++ show txs
    show (ReqBlockMsg h) = "ReqBlockMsg " ++ show h
