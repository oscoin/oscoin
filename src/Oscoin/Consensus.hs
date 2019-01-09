module Oscoin.Consensus
    ( nakamotoConsensus
    , simpleConsensus
    , validateBlockchain
    , validateBlockSize
    , module Oscoin.Consensus.Mining
    , module Oscoin.Consensus.Types
    ) where

import           Oscoin.Prelude

import qualified Data.ByteString.Lazy as LBS

import           Codec.Serialise (Serialise, serialise)
import           Oscoin.Consensus.Mining
import           Oscoin.Consensus.Nakamoto (nakamotoConsensus)
import           Oscoin.Consensus.Simple (simpleConsensus)
import           Oscoin.Consensus.Types

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain

import           GHC.Exts (IsList(fromList))

-- | Validate a 'Blockchain' with the given validation function. Returns 'Right'
-- when valid.
validateBlockchain :: Validate tx s
                   -> Blockchain tx s
                   -> Either ValidationError ()
validateBlockchain validateBlock (Blockchain (blk :| [])) =
    validateBlock [] blk
validateBlockchain validateBlock (Blockchain (blk :| blks)) =
    validateBlock blks blk *> validateBlockchain validateBlock (fromList blks)


-- | Validates the size (in bytes) for a block, comparing the serialised size
-- with the maximum allowed value as read from the configuration file.
validateBlockSize :: (Serialise tx, Serialise s)
                  => Consensus.Config
                  -> Block tx s
                  -> Either ValidationError ()
validateBlockSize config block
    | actualSize <- LBS.length (serialise block)
    , actualSize > fromIntegral (Consensus.maxBlockSize config) =
        Left $ BlockExceededMaximumSize (fromIntegral actualSize)
    | otherwise = Right ()
