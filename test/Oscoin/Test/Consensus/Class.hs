module Oscoin.Test.Consensus.Class
    ( MonadProtocol (..)
    , Msg (..)
    , tickM
    , module Oscoin.Clock
    ) where

import           Oscoin.Prelude hiding (show)

import           Oscoin.Clock (MonadClock(..), Tick)
import           Oscoin.Consensus.BlockStore.Class (MonadBlockStore)
import           Oscoin.Crypto.Blockchain (showBlockDigest)
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)
import           Oscoin.Node.Mempool.Class (MonadMempool(..))

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

class (MonadMempool tx m, MonadBlockStore tx s m) => MonadProtocol tx s m | m -> tx s where
    mineM      :: Tick -> m (Maybe (Block tx ()))
    reconcileM :: Tick -> m [BlockHash]

tickM :: MonadProtocol tx s m => Tick -> m [Msg tx]
tickM t = do
    blk  <- mineM t
    reqs <- reconcileM t
    pure $ maybeToList (BlockMsg <$> blk) <> (ReqBlockMsg <$> reqs)
