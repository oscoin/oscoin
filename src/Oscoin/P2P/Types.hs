{-# LANGUAGE UndecidableInstances #-}

module Oscoin.P2P.Types
    ( NodeId
    , mkNodeId
    , fromNodeId

    , SelfAddr
    , SeedAddr
    , NodeAddr(..)
    , readNodeAddr

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
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Hashable (Hashable(..))
import           Data.IP (IP(..))
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

-- | 'NodeAddr' of this node. Must specify a 'NodeId'.
type SelfAddr = NodeAddr Identity

-- | 'NodeAddr' of a seed node. May specify a 'NodeId'.
type SeedAddr = NodeAddr Maybe

data NodeAddr f c = NodeAddr
    { nodeId   :: f (NodeId c)
    , nodeHost :: HostName
    , nodePort :: PortNumber
    }

deriving instance Eq   (f (NodeId c)) => Eq   (NodeAddr f c)
deriving instance Show (f (NodeId c)) => Show (NodeAddr f c)

-- | Read a 'NodeAddr Maybe' from a 'String'. For use in CLI parsers.
--
-- The 'NodeId' is always 'Nothing'.
--
-- The input string is expected to be a @:@-separated pair of host : port, where
-- host may be an IP address or hostname. If host is an IPv6 address, it must be
-- enclosed in square brackets as per <https://tools.ietf.org/html/rfc3986#section-3.2.2 RFC 3986, Section 3.2.2>
-- in order to delimit it from the port number.
--
-- Examples:
--
-- >>> readNodeAddr "[2001:db8:00:00:00:00:00:01]:42"
-- Right (NodeAddr {nodeId = Nothing, nodeHost = "2001:db8::1", nodePort = 42})
--
-- >>> readNodeAddr "[2001:db8::01]:42"
-- Right (NodeAddr {nodeId = Nothing, nodeHost = "2001:db8::1", nodePort = 42})
--
-- >>> readNodeAddr "127.0.0.1:42"
-- Right (NodeAddr {nodeId = Nothing, nodeHost = "127.0.0.1", nodePort = 42})
--
-- >>> readNodeAddr "goo.gl:42"
-- Right (NodeAddr {nodeId = Nothing, nodeHost = "goo.gl", nodePort = 42})
--
readNodeAddr :: String -> Either String (NodeAddr Maybe c)
readNodeAddr s = do
    (h, more) <- (first show <$> readIP s) <|> readHost s
    p         <- note "Invalid port number" (readMaybe more)
    pure NodeAddr
        { nodeId   = Nothing
        , nodeHost = h
        , nodePort = p
        }
  where
    readIP :: String -> Either String (IP, String)
    readIP ('[' : more) =
        case break (== ']') more of
            (xs, ']' : ':' : rest) ->
                case reads xs of
                    [(ipv6, [])] -> Right (IPv6 ipv6, rest)
                    _            -> Left "Invalid IPv6"
            _                      -> Left "Missing closing ']' for IPv6"

    readIP s' = case reads s' of
        [(ipv4, ':' : rest)] -> Right (IPv4 ipv4, rest)
        _                    -> Left "Invalid IP"

    readHost :: String -> Either String (HostName, String)
    readHost s' = case break (== ':') s' of
        (h, ':' : rest) -> Right (h, rest)
        _               -> Left "Missing ':' separator after host name"

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
