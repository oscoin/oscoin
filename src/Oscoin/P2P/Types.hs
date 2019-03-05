{-# LANGUAGE UndecidableInstances #-}
module Oscoin.P2P.Types
    ( NodeId
    , mkNodeId
    , fromNodeId

    , NodeAddr(..)

    , Msg(..)
    , MsgId(..)

    , HandshakeEvent(..)
    , ConversionError(..)

    -- * Formatters
    , fmtLogConversionError
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)
import           Oscoin.Crypto.Hash (Hash, Hashed)
import           Oscoin.Crypto.PubKey (PK)
import           Oscoin.Telemetry.Logging as Log

import qualified Network.Gossip.IO.Peer as Gossip (Peer)

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
import qualified Generics.SOP as SOP
import           Network.Socket (HostName, PortNumber, SockAddr)

newtype NodeId c = NodeId { fromNodeId :: PK c }
    deriving (Generic)

deriving instance Show (PK c)     => Show (NodeId c)
deriving instance FromJSON (PK c) => FromJSON (NodeId c)
deriving instance ToJSON (PK c)   => ToJSON (NodeId c)

deriving instance Eq (PK c)  => Eq (NodeId c)
deriving instance Ord (PK c) => Ord (NodeId c)

deriving instance (Hashable (PK c))  => Hashable  (NodeId c)
instance (Serialise (PK c))          => Serialise (NodeId c)

mkNodeId :: PK c -> NodeId c
mkNodeId = NodeId

data NodeAddr c = NodeAddr
    { nodeId   :: NodeId c
    , nodeHost :: HostName
    , nodePort :: PortNumber
    }

deriving instance (Eq (PK c)) => Eq (NodeAddr c)

instance FromJSON (PK c) => FromJSON (NodeAddr c) where
    parseJSON = withObject "NodeAddr" $ \o -> do
        nodeId   <- o .: "nodeId"
        nodeHost <- o .: "host"
        nodePort <- toEnum <$> o .: "port"
        pure NodeAddr{..}

instance ToJSON (PK c) => ToJSON (NodeAddr c) where
    toJSON NodeAddr{..} = object
        [ "nodeId" .= nodeId
        , "host"   .= nodeHost
        , "port"   .= fromEnum nodePort
        ]

data Msg c tx s =
      BlockMsg (Block c tx s)
    | TxMsg    tx
    deriving (Generic)

deriving instance (Eq (Hash c), Eq tx, Eq s) => Eq (Msg c tx s)
instance ( Serialise (Block c tx s)
         , Serialise tx
         , Serialise s
         )
         => Serialise (Msg c tx s)

data MsgId c tx =
      BlockId (BlockHash c)
    | TxId    (Hashed c tx)
    deriving (Generic)

deriving instance Eq (BlockHash c) => Eq (MsgId c tx)
instance (Serialise (BlockHash c), Serialise tx) => Serialise (MsgId c tx)

data HandshakeEvent n =
      HandshakeError    SockAddr SomeException
    | HandshakeComplete (Gossip.Peer n)

data ConversionError =
      DeserialiseFailure CBOR.DeserialiseFailure
    | IdPayloadMismatch
    deriving (Generic)

instance SOP.Generic ConversionError
instance SOP.HasDatatypeInfo ConversionError

-- | Formats the input 'ConversionError' in a form suitable for logging.
fmtLogConversionError :: Format r (ConversionError -> r)
fmtLogConversionError = Log.ferror toErrorMsg
  where
    toErrorMsg :: ConversionError -> Text
    toErrorMsg (DeserialiseFailure f) = toS $ displayException f
    toErrorMsg IdPayloadMismatch      = "The payload ID didn't match"
