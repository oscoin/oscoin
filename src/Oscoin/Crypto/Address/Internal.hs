{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Crypto.Address.Internal where

import           Oscoin.Prelude

import           Oscoin.Configuration
import           Oscoin.Crypto.PubKey

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- | An 'Address' is broken down as follows:
--
-- * /5/ bytes for the version prefix, so allocated:
--
-- @| 00 | 00 | 00 | 00 | 00 |
--   ^    ^     ^   ^    ^
--   |    |     |   |    |_ reserved (unused)
--   |    |     |   |_ reserved (unused)
--   |    |     |_ address format (only cbor for now)
--   |    |_ address type (mainnet, testnet, devnet, multisig, etc)
--   |_  protocol version (bumped on hard forks)@
--
-- * /23/ bytes for the CBOR-encoded address payload
--
-- * /6/ bytes for the checksum, computed from the @prefix + payload@.
--
data Address c = Address
    { addressPrefix  :: AddressPrefix
    , addressPayload :: AddressPayload c
    }

deriving instance Eq (PublicKey c)   => Eq (Address c)
deriving instance Ord (PublicKey c)  => Ord (Address c)
deriving instance Show (PublicKey c) => Show (Address c)

data AddressPrefix = AddressPrefix
    { protocolVersion :: ProtocolVersion
    , addressType     :: AddressType
    , addressFormat   :: AddressFormat
    } deriving (Eq, Ord, Show)

-- | A 'ProtocolVersion' is a byte representing the current version of the
-- Oscoin protocol running. This makes it possible to issue forks of the system
-- by simply acting on this value, so that nodes running the old protocol
-- won't be able to use addresses from the new fork. Conversely, nodes
-- running the fork can be programmed to support both formats.
newtype ProtocolVersion =
    ProtocolVersion ProtocolVersionTag
    deriving (Eq, Ord, Show)

data ProtocolVersionTag =
      ProtocolVersion_V1
      deriving (Eq, Ord, Show)

-- | The 'AddressType', i.e. the qualifying type of this 'Address'.
-- Example:
--
-- 0x01 Mainnet
-- 0x02 Testnet
-- 0x03 Devnet
-- ..
newtype AddressType =
    AddressType Network
    deriving (Eq, Ord, Show)

-- | The 'AddressFormat' allows for flexibility in the 'Address' payload, i.e.
-- it specifies the serialisation format used to encode the 'AddressPayload'.
-- For now we have simply:
--
-- 0x01 -- CBOR
--
newtype AddressFormat =
    AddressFormat AddressFormatTag
    deriving (Eq, Ord, Show)

data AddressFormatTag =
    AddressFormatTag_CBOR
    deriving (Eq, Ord, Show)

-- | The 'AddressPayload' is (currently) simply a 'PublicKey'.
--
data AddressPayload c where
    AddressPayload_V0 :: PublicKey c -> AddressPayload c

deriving instance Eq (PublicKey c)   => Eq (AddressPayload c)
deriving instance Ord (PublicKey c)  => Ord (AddressPayload c)
deriving instance Show (PublicKey c) => Show (AddressPayload c)

newtype Checksum = Checksum { fromChecksum :: ByteString }
    deriving (Show, Eq)
