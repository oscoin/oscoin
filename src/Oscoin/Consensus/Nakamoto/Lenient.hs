{- | A version of the 'Nakamoto' consensus more lenient on the validation
-- aspect, and suitable for development.
-}

module Oscoin.Consensus.Nakamoto.Lenient
    ( nakamotoConsensusLenient
    , validateLenient
    , mineLenient
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Nakamoto
                 ( PoW
                 , blockTime
                 , chainDifficulty
                 , nakamotoConsensus
                 , validateBasic
                 )
import           Oscoin.Consensus.Types
import           Oscoin.Consensus.Validation
                 ( validateDifficulty
                 , validateHeight
                 , validateParentHash
                 , validateTimestamp
                 )
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash (Hash, Hashable)
import           Oscoin.Node.Mempool.Class
import qualified Oscoin.Node.Mempool.Class as Mempool
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Time (seconds)

import           Codec.Serialise (Serialise)
import           Control.Monad.Except (liftEither, runExcept)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Data.ByteArray (ByteArrayAccess)

-- | Like 'nakamotoConsensus', but uses a looser validation function, more
-- suitable for development.
nakamotoConsensusLenient
    :: forall c tx m.
       ( MonadIO m
       , MonadMempool c tx m
       , Serialise tx
       , Serialise (Beneficiary c)
       , AuthTree.MerkleHash (Hash c)
       , ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       )
    => Telemetry.Tracer m
    -> Word8 -- ^ Block time lower bound in seconds (see 'mineLenient')
    -> Consensus c tx PoW m
nakamotoConsensusLenient probed blkTimeLower =
    let nak = nakamotoConsensus probed :: Consensus c tx PoW m
     in nak { cValidate = validateLenient
            , cMiner    = mineLenient blkTimeLower (cMiner nak)
            }

-- | A more lenient validation function, where the \"age\" of block with
-- respect to the parent is not checked.
validateLenient
    :: ( ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , AuthTree.MerkleHash (Hash c)
       , Serialise tx
       , Serialise (Beneficiary c)
       )
    => Validate c tx PoW
validateLenient [] blk =
    validateBasic blk
validateLenient prefix@(parent:_) blk = runExcept $ do
    validateHeight     parent blk
    validateParentHash parent blk
    validateDifficulty chainDifficulty prefix blk
    validateTimestamp  parent blk
    liftEither (validateBasic blk)

-- | Modify mining behaviour when the network does not produce transactions.
--
-- Currently, this simply delays mining an empty block by 'blockTime' or 'Word8'
-- seconds, whichever is lower. Mainly useful to avoid busy looping in an idle
-- network, while retaining correctness.
--
mineLenient
    :: forall c tx m. (MonadIO m, MonadMempool c tx m)
    => Word8 -- Block time lower bound in seconds
    -> Miner c PoW m
    -> Miner c PoW m
mineLenient blkTimeLowerSecs inner getBlocks unsealedBlock = do
    nTxs <- Mempool.numTxs
    when (nTxs == 0) $
        let
            blkTime   = min (fromIntegral blkTimeLowerSecs * seconds) blockTime
            blkTimeMu = fromIntegral blkTime `div` 1000
         in
            liftIO $ threadDelay blkTimeMu

    inner getBlocks unsealedBlock
