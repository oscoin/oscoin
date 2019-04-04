{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Crypto.Address.Internal where

import           Oscoin.Prelude

import           Oscoin.Configuration
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.PubKey

import           Codec.CBOR.ByteArray (ByteArray)
import           Data.Tagged

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
    } deriving (Eq, Show)

data AddressPrefix = AddressPrefix
    { protocolVersion :: ProtocolVersion
    , addressType     :: AddressType
    , addressFormat   :: AddressFormat
    } deriving (Eq, Show)

-- | A 'ProtocolVersion' is a byte representing the current version of the
-- Oscoin protocol running. This makes it possible to issue forks of the system
-- by simply acting on this value, so that nodes running the old protocol
-- won't be able to use addresses from the new fork. Conversely, nodes
-- running the fork can be programmed to support both formats.
newtype ProtocolVersion =
    ProtocolVersion ProtocolVersionTag
    deriving (Eq, Show)

data ProtocolVersionTag =
      ProtocolVersion_V1
      deriving (Eq, Show)

-- | The 'AddressType', i.e. the qualifying type of this 'Address'.
-- Example:
--
-- 0x01 Mainnet
-- 0x02 Testnet
-- 0x03 Devnet
-- ..
newtype AddressType =
    AddressType AddressTypeTag
    deriving (Eq, Show)

data AddressTypeTag =
      AddressType_Mainnet
    | AddressType_Testnet
    | AddressType_Devnet
    deriving (Eq, Show)

-- | The 'AddressFormat' allows for flexibility in the 'Address' payload, i.e.
-- it specifies the serialisation format used to encode the 'AddressPayload'.
-- For now we have simply:
--
-- 0x01 -- CBOR
--
newtype AddressFormat =
    AddressFormat AddressFormatTag
    deriving (Eq, Show)

data AddressFormatTag =
    AddressFormatTag_CBOR
    deriving (Eq, Show)

-- | The 'AddressPayload' is simply the hash of a 'PublicKey'.
--
newtype AddressPayload c =
    AddressPayload {
       -- NOTE(adn): The use of 'Tagged' here is due to the fact we cannot encode
       -- directly 'ShortHashed c (PublicKey c)' as there is no isomorphism between
       -- a 'ShortHashed' and a 'ByteString', i.e. when decoding we wouldn't otherwise
       -- be able to go from a 'ByteString' to a 'ShortHashed' in a meaningful way.
        getAddressPayload :: Tagged (ShortHashed c (PublicKey c)) ByteArray
    }
    deriving (Eq, Show)

newtype Checksum = Checksum { fromChecksum :: ByteString }
    deriving (Show, Eq)

{------------------------------------------------------------------------------
  Miscellanea functions
------------------------------------------------------------------------------}

configToAddressTypeTag :: Network -> Maybe AddressTypeTag
configToAddressTypeTag = \case
  Mainnet -> Just AddressType_Mainnet
  Testnet -> Just AddressType_Testnet
  Devnet  -> Just AddressType_Devnet
  -- NOTE(adn) The 'Somenet' constructor is not exported, which makes this
  -- pattern-match non-exhaustive.
  _       -> Nothing

