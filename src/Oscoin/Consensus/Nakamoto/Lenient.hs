{- | A version of the 'Nakamoto' consensus more lenient on the validation
-- aspect, and suitable for development.
-}

module Oscoin.Consensus.Nakamoto.Lenient
    ( nakamotoConsensusLenient
    , validateLenient
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Nakamoto
                 (PoW, chainDifficulty, nakamotoConsensus, validateBasic)
import           Oscoin.Consensus.Types
import           Oscoin.Consensus.Validation
                 (validateDifficulty, validateParentHash, validateTimestamp)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Hash (Hash, Hashable)

import           Codec.Serialise (Serialise)
import           Control.Monad.Except (liftEither, runExcept)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Data.ByteArray (ByteArrayAccess)

-- | Like 'nakamotoConsensus', but uses a looser validation function, more
-- suitable for development.
nakamotoConsensusLenient
    :: ( AuthTree.MerkleHash (Hash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , Hashable c (BlockHeader c Unsealed)
       , ByteArrayAccess (BlockHash c)
       , Monad m
       , Serialise tx
       )
    => Consensus c tx PoW m
nakamotoConsensusLenient = nakamotoConsensus { cValidate = validateLenient }

-- | A more lenient validation function, where the \"age\" of block with
-- respect to the parent is not checked.
validateLenient
    :: ( ByteArrayAccess (BlockHash c)
       , Hashable c (BlockHeader c (Sealed c PoW))
       , AuthTree.MerkleHash (Hash c)
       , Serialise tx
       )
    => Validate c tx PoW
validateLenient [] blk =
    validateBasic blk
validateLenient prefix@(parent:_) blk = runExcept $ do
    validateParentHash parent blk
    validateDifficulty chainDifficulty prefix blk
    validateTimestamp  parent blk
    liftEither (validateBasic blk)
