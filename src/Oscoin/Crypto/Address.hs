-- | An address format for the Oscoin blockchain loosely based on Parity's SS58
-- and on BTC's Base58Check.
--
-- See @docs/address_format.md@ for an extensive documentation.
--

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Oscoin.Crypto.Address
    ( Address(addressPrefix, addressPayload)
    , AddressPrefix
    , AddressPayload

    -- * Creating an Address
    , fromPublicKey

    -- * Parsing binary blobs back into addresses
    , decodeAddress

    -- * Pretty-printing an Address
    , renderAddress
    , fmtAddress
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Configuration as Config
import           Oscoin.Crypto.Address.Internal
import           Oscoin.Crypto.Address.Serialisation as Serialisation
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.PubKey

import qualified Codec.CBOR.ByteArray as CBOR
import qualified Codec.Serialise as CBOR
import           Data.ByteArray (ByteArrayAccess, convert)
import           Data.ByteString.BaseN
                 (decodeBase32z, encodeBase32z, encodedText)
import           Data.Tagged
import qualified Formatting as F

{------------------------------------------------------------------------------
    Creating an Address
------------------------------------------------------------------------------}

-- | Creates an 'Address' given a 'Network' configuration and a 'PublicKey'.
fromPublicKey
    :: forall c.
       ( ByteArrayAccess (ShortHash c)
       , Hashable c (PublicKey c)
       )
    => Config.Network
    -- ^ The 'Network' the node is running on.
    -> PublicKey c
    -- ^ A 'PublicKey'.
    -> Address c
fromPublicKey network pk =
    let
        prefix = AddressPrefix
            { addressFormat   = AddressFormat AddressFormatTag_CBOR
            , addressType     = AddressType network
            , protocolVersion = ProtocolVersion ProtocolVersion_V1
            }
        payload = Tagged . CBOR.fromByteString . convert . shortHash @c $ pk
     in
        Address prefix (AddressPayload payload)

-- | Parses a base32z-encoded binary block back into an 'Address'.
decodeAddress
    :: ( CBOR.Serialise  (ShortHash c)
       , ByteArrayAccess (ShortHash c)
       )
    => ByteString
    -> Either Serialisation.DeserializeError (Address c)
decodeAddress base32zBlob =
    case decodeBase32z base32zBlob of
        Nothing -> Left $ Serialisation.InvalidBase32zEncoding base32zBlob
        Just bs -> Serialisation.deserializeAddress bs

{------------------------------------------------------------------------------
    Using an Address
------------------------------------------------------------------------------}

-- | Renders an 'Address' into its textual form (i.e. a z-base-32-encoded
-- blob).
renderAddress
    :: ( CBOR.Serialise (ShortHash c)
       , ByteArrayAccess (ShortHash c)
       )
    => Address c
    -> Text
renderAddress = F.sformat fmtAddress

-- | Formats an 'Address'.
-- Nb. This might be quite heavy as it goes through serialisation.
fmtAddress
    :: ( CBOR.Serialise (ShortHash c)
       , ByteArrayAccess (ShortHash c)
       )
    => F.Format r (Address c -> r)
fmtAddress = F.mapf (encodedText . encodeBase32z . serializeAddress) F.stext
