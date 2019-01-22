module Oscoin.P2P.Types
    ( NodeId
    , mkNodeId
    , fromNodeId

    , NodeAddr(..)

    , Msg(..)
    , MsgId(..)
    , ConversionError(..)

    -- * Formatters
    , fmtLogConversionError
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Telemetry.Logging as Log

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import           Data.Aeson
                 ( FromJSON
                 , ToJSON
                 , object
                 , parseJSON
                 , toJSON
                 , withObject
                 , (.:)
                 , (.=)
                 )
import           Data.Hashable (Hashable(..))
import           Formatting as F
import           Network.Socket (HostName, PortNumber)

newtype NodeId = NodeId { fromNodeId :: PublicKey }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Hashable  NodeId
instance Serialise NodeId

mkNodeId :: PublicKey -> NodeId
mkNodeId = NodeId

data NodeAddr = NodeAddr
    { nodeId   :: NodeId
    , nodeHost :: HostName
    , nodePort :: PortNumber
    } deriving (Eq)

instance FromJSON NodeAddr where
    parseJSON = withObject "NodeAddr" $ \o -> do
        nodeId   <- o .: "nodeId"
        nodeHost <- o .: "host"
        nodePort <- toEnum <$> o .: "port"
        pure NodeAddr{..}

instance ToJSON NodeAddr where
    toJSON NodeAddr{..} = object
        [ "nodeId" .= nodeId
        , "host"   .= nodeHost
        , "port"   .= fromEnum nodePort
        ]

data Msg tx s =
      BlockMsg (Block tx s)
    | TxMsg    tx
    deriving (Eq, Generic)

instance (Serialise tx, Serialise s) => Serialise (Msg tx s)

data MsgId tx =
      BlockId BlockHash
    | TxId    (Hashed tx)
    deriving (Eq, Generic)

instance Serialise tx => Serialise (MsgId tx)

data ConversionError =
      DeserialiseFailure CBOR.DeserialiseFailure
    | IdPayloadMismatch

-- | Formats the input 'ConversionError' in a form suitable for logging.
fmtLogConversionError :: Format r (ConversionError -> r)
fmtLogConversionError =  (Log.ftag "error_class" % F.mapf toErrorClass fquoted)
                      <> (" " % Log.ftag "error_message" % F.mapf toErrorMsg fquoted)

  where
      toErrorClass :: ConversionError -> Text
      toErrorClass (DeserialiseFailure _) = "deserialise_failure"
      toErrorClass IdPayloadMismatch      = "id_payload_mismatch"

      toErrorMsg :: ConversionError -> Text
      toErrorMsg (DeserialiseFailure f) = toS $ displayException f
      toErrorMsg IdPayloadMismatch      = "The payload ID didn't match"
