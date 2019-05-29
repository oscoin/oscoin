{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'GenesisParameters' data type that is used
-- to build the genesis block. 'GenesisParameters' can be serialized to
-- and from JSON.
module Oscoin.Crypto.Blockchain.Genesis
    ( GenesisParameters(..)
    , buildGenesisBlock
    , createGenesisParameters
    ) where

import           Oscoin.Prelude

import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import           Oscoin.Consensus.Nakamoto (PoW(..), findPoW)
import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.Tx
import qualified Oscoin.Time as Time

import           Codec.Serialise
import           Control.Monad.Fail
import           Data.ByteArray (ByteArrayAccess)


-- | Set of parameters that uniquely define the genesis block.
data GenesisParameters c = GenesisParameters
    { gpBeneficiary     :: Beneficiary c
    , gpTimestamp       :: Timestamp
    , gpStartDifficulty :: Difficulty
    , gpPoW             :: PoW
    } deriving (Generic)

deriving instance (Eq (Beneficiary c)) => Eq (GenesisParameters c)
deriving instance (Show (Beneficiary c)) => Show (GenesisParameters c)


instance (Aeson.ToJSON (Crypto.ShortHash c)) => Aeson.ToJSON (GenesisParameters c) where
    toJSON GenesisParameters{..} = Aeson.object
        [ "timestamp" .= Time.sinceEpoch gpTimestamp
        , "startDifficulty" .= prettyDifficulty gpStartDifficulty
        , "beneficiary" .= gpBeneficiary
        , "pow" .= nonce
        ]
      where
        PoW nonce = gpPoW

instance (Aeson.FromJSON (Crypto.ShortHash c)) => Aeson.FromJSON (GenesisParameters c) where
    parseJSON = Aeson.withObject "GenesisParameters" $ \o -> do
        gpTimestamp <- Time.fromEpoch <$> o .: "timestamp"
        gpStartDifficulty <- parseJsonDifficulty =<< o .: "startDifficulty"
        gpBeneficiary <- o .: "beneficiary"
        gpPoW <- PoW <$> o .: "pow"
        pure $ GenesisParameters{..}
      where
        parseJsonDifficulty =
            Aeson.withText "Difficulty" $ \t ->
                case parseDifficulty t of
                    Just d  -> pure d
                    Nothing -> fail "Error decoding difficulty"


-- | Build a genesis block from the given parameters. The block is
-- intrinsically valid. It’s state hash references the 'mempty'
-- 'LegacyTxState'
buildGenesisBlock
    :: forall c tx. ( Serialise (Crypto.Hash c)
       , Crypto.HasHashing c
       , Serialise (Beneficiary c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       )
    => GenesisParameters c -> Block c tx (Sealed c PoW)
buildGenesisBlock gp = mkBlock blockHeader (gpBeneficiary gp) []
  where
    blockHeader = BlockHeader
        { blockDataHash = noTxDataHash (gpBeneficiary gp)
        , blockStateHash = initialStateHash
        , blockTimestamp = gpTimestamp gp
        , blockHeight = 0
        , blockPrevHash = Crypto.zeroHash
        , blockTargetDifficulty = gpStartDifficulty gp
        , blockSeal = SealedWith (gpPoW gp)
        }

-- | Create 'GenesisParameters' for the given data. Computes the proof
-- of work so that the block generated
createGenesisParameters
    :: ( Serialise (Crypto.Hash c)
       , Crypto.HasHashing c
       , ByteArrayAccess (BlockHash c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       , Serialise (Beneficiary c)
       ) => Beneficiary c -> Timestamp -> Difficulty -> Maybe (GenesisParameters c)
createGenesisParameters gpBeneficiary gpTimestamp gpStartDifficulty = do
    gpPoW <- findPoW blockHeader
    pure $ GenesisParameters{..}
  where
    blockHeader = BlockHeader
        { blockDataHash = noTxDataHash gpBeneficiary
        , blockStateHash = initialStateHash
        , blockTimestamp = gpTimestamp
        , blockHeight = 0
        , blockPrevHash = Crypto.zeroHash
        , blockTargetDifficulty = gpStartDifficulty
        , blockSeal = Unsealed
        }


-- Internal ---------------------------------------------------------


-- | Computes the hash of block data where 'blockDataTxs' is empty.
noTxDataHash
    :: ( Serialise (Beneficiary c)
       , AuthTree.MerkleHash (Crypto.Hash c)
       )
    => Beneficiary c -> Crypto.Hash c
noTxDataHash beneficiary = hashData $ mkBlockData beneficiary txs
  where
    -- We satisfy the @'Serialise' tx@ constraint of 'hashData' by
    -- using '()' as @tx@. Since we are using an empty list the hash is
    -- actually independent of @tx@.
    txs = [] :: [()]


-- | Initial state referenced by the genesis block. We hardcode this
-- for now.
initialStateHash :: Crypto.HasHashing c => Crypto.Hash c
initialStateHash = Crypto.fromHashed $ Crypto.hash initialState

initialState :: LegacyTxState
initialState = mempty
