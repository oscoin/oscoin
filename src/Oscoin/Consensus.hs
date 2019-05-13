module Oscoin.Consensus
    ( nakamotoConsensus
    , nakamotoConsensusLenient
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
import           Oscoin.Consensus.Nakamoto.Lenient (nakamotoConsensusLenient)
import           Oscoin.Consensus.Types

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash (Hash, Hashable)

import           GHC.Exts (IsList(fromList))

-- | Validate a 'Blockchain' with the given validation function. Returns 'Right'
-- when valid.
validateBlockchain :: Validate c tx s
                   -> Blockchain c tx s
                   -> Either (ValidationError c) ()
validateBlockchain validateBlock (Blockchain (blk :| [])) =
    validateBlock [] blk
validateBlockchain validateBlock (Blockchain (blk :| blks)) =
    validateBlock blks blk *> validateBlockchain validateBlock (fromList blks)


-- | Validates the size (in bytes) of a block, comparing the serialized size
-- with the maximum allowed value as read from the configuration.
validateBlockSize
    :: ( Serialise tx
       , Hashable c (BlockHeader c s)
       , Serialise s
       , Serialise (Hash c)
       , Serialise (BlockData c tx)
       )
    => Consensus.Config
    -> Block c tx s
    -> Either (ValidationError c) ()
validateBlockSize config block
    | actualSize <- LBS.length (serialise block)
    , actualSize > fromIntegral (Consensus.maxBlockSize config) =
        Left $ InvalidBlockSize (fromIntegral actualSize)
    | otherwise = Right ()
