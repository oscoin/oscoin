{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.P2P.Types
    ( Network(Mainnet, Testnet, Devnet)
    , pattern Somenet
    , readNetwork
    , readNetworkText
    , renderNetwork
    , randomNetwork
    , fromPhysicalNetwork

    , NodeId
    , mkNodeId
    , fromNodeId

    , NodeInfo
    , mkNodeInfo
    , nodeHttpApiAddr
    , nodeNodeId

    , Addr
    , mkAddr
    , addrHost
    , addrPort

    , Host
    , numericHost
    , namedHost
    , renderHost
    , readHost
    , hostEither
    , hostToEither
    , eitherToHost
    , hostToHostName

    , Hostname
    , renderHostname
    , readHostnameText
    , hostnameToDomain
    , domainToHostname

    , SelfInfo
    , SeedInfo
    , BootstrapInfo(..)
    , readBootstrapInfo
    , showBootstrapInfo

    , Msg(..)
    , MsgId(..)

    , HandshakeEvent(..)
    , ConversionError(..)

    -- * Formatters
    , fmtLogConversionError
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Configuration as Logical (Network(..))
import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)
import           Oscoin.Crypto.Hash (Hash, Hashed)
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Telemetry.Logging as Log

import qualified Network.Gossip.IO.Peer as Gossip (Peer)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Monad.Fail (fail)
import           Control.Monad.Trans.Writer (execWriterT, tell)
import           Data.Char (isAlphaNum)
import           Data.Hashable (Hashable(..))
import           Data.IP (IP(..))
import           Data.Profunctor (Profunctor(dimap))
import           Data.String (IsString(..))
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as V.Unboxed
import           Formatting as F
import qualified Generics.SOP as SOP
import qualified Network.DNS as DNS
import           Network.Socket (PortNumber, SockAddr)
import qualified Network.Socket as Network
import           System.Random (RandomGen, randomR)

-- | The name of the overlay network.
--
-- This is, in a way, the \"physical\" network a node joins, although physical
-- isolation is not implied. The overlay network typically matches the
-- \"logical\" oscoin network as defined by "Oscoin.Configuration.Network".
-- Additionally, ad-hoc networks ('Somenet'') can be created, which map to the
-- logical "Oscoin.Configuration.Devnet".
--
-- Ad-hoc networks must be valid domain name labels (RFC 1035, RFC 1123, RFC
-- 2181), and can only be created using 'readNetwork', 'readNetworkText', or
-- 'randomNetwork'. To allow exhaustive pattern matching, the view pattern
-- 'Somenet' is exported.
--
data Network =
      Mainnet
    | Testnet
    | Devnet
    | Somenet' Text
    deriving (Eq, Show)

pattern Somenet :: Text -> Network
pattern Somenet x <- Somenet' x

{-# COMPLETE Mainnet, Testnet, Devnet, Somenet #-}

instance Serialise Network where
    encode Mainnet      = CBOR.encodeListLen 1 <> CBOR.encodeWord 0
    encode Testnet      = CBOR.encodeListLen 1 <> CBOR.encodeWord 1
    encode Devnet       = CBOR.encodeListLen 1 <> CBOR.encodeWord 2
    encode (Somenet' x) = CBOR.encodeListLen 2 <> CBOR.encodeWord 3 <> CBOR.encode x

    decode = do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeWord
        case (len, tag) of
            (1, 0) -> pure Mainnet
            (1, 1) -> pure Testnet
            (1, 2) -> pure Devnet
            (2, 3) -> CBOR.decode >>= either fail pure . readNetworkText
            _      -> fail "Oscoin.P2P.Types.Network: Unknown length/tag"

instance IsString Network where
    fromString = either (panic . toS) identity . readNetwork

renderNetwork :: Network -> Text
renderNetwork Mainnet      = "mainnet"
renderNetwork Testnet      = "testnet"
renderNetwork Devnet       = "devnet"
renderNetwork (Somenet' x) = x

readNetwork :: String -> Either String Network
readNetwork = readNetworkText . toS

readNetworkText :: Text -> Either String Network
readNetworkText "mainnet" = pure Mainnet
readNetworkText "testnet" = pure Testnet
readNetworkText "devnet"  = pure Devnet
readNetworkText t
  | T.length t > 63            = Left "Network name longer than 63 characters"
  | Just ('-',_) <- T.uncons t = Left "Network name cannot start with hyphen"
  | Just c <- invalidChar t    = Left $ "Invalid character in network name: " <> [c]
  | otherwise                  = pure $ Somenet' (T.toLower t)
  where
    invalidChar = T.find $ \c -> c /= '-' && not (isAlphaNum c)

randomNetwork :: RandomGen g => g -> Network
randomNetwork g = Somenet' . flip evalState g . execWriterT $ do
    lift (randC firstChar) >>= tell . T.singleton
    num <- lift . state $ randomR (0, 62)
    replicateM num $
        lift (randC labelChars) >>= tell . T.singleton
  where
    randC cs = state $
        first (cs V.Unboxed.!) . randomR (0, V.Unboxed.length cs - 1)

    labelChars = V.Unboxed.fromList "abcdefghiklmnopqrstuvwxyz0123456789-"
    firstChar  = V.Unboxed.init labelChars

fromPhysicalNetwork :: Network -> Logical.Network
fromPhysicalNetwork = \case
    Mainnet    -> Logical.Mainnet
    Testnet    -> Logical.Testnet
    Devnet     -> Logical.Devnet
    Somenet'{} -> Logical.Devnet

-- | A cryptographically secure identifier for a logical peer.
newtype NodeId c = NodeId { fromNodeId :: PublicKey c }
    deriving (Generic)

deriving instance Show (PublicKey c) => Show (NodeId c)
deriving instance Eq (PublicKey c)  => Eq (NodeId c)
deriving instance Ord (PublicKey c) => Ord (NodeId c)

deriving instance (Hashable (PublicKey c))  => Hashable  (NodeId c)
instance (Serialise (PublicKey c))          => Serialise (NodeId c)

mkNodeId :: PublicKey c -> NodeId c
mkNodeId = NodeId

data Addr = Addr
    { addrHost :: Host
    , addrPort :: PortNumber
    } deriving (Show, Eq, Ord)

mkAddr :: Host -> PortNumber -> Addr
mkAddr = Addr

instance Hashable Addr where
    hashWithSalt s Addr{..} =
        hashWithSalt  s (renderHost addrHost)
        `hashWithSalt`  fromEnum addrPort

instance Serialise Addr where
    encode Addr{..} =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 0
        <> CBOR.encode addrHost
        <> CBOR.encode (fromEnum addrPort)
    decode = do
        CBOR.decodeListLenOf 3
        tag <- CBOR.decodeWord
        case tag of
            0 -> Addr <$> CBOR.decode
                             <*> map toEnum CBOR.decode
            _ ->
                fail "Error decoding Addr: unknown tag"

data NodeInfo c = NodeInfo
    { nodeHttpApiAddr :: Addr
    , nodeNodeId      :: NodeId c
    } deriving (Generic)

deriving instance Show (NodeId c) => Show (NodeInfo c)
deriving instance Eq (NodeId c)  => Eq (NodeInfo c)
deriving instance Ord (NodeId c) => Ord (NodeInfo c)

instance Hashable (NodeId c) => Hashable (NodeInfo c) where
    hashWithSalt s NodeInfo{..} =
        hashWithSalt s nodeNodeId `hashWithSalt` nodeHttpApiAddr

instance (Serialise (NodeId c)) => Serialise (NodeInfo c) where
    encode NodeInfo{..} =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 0
        <> CBOR.encode nodeHttpApiAddr
        <> CBOR.encode nodeNodeId
    decode = do
        CBOR.decodeListLenOf 3
        tag <- CBOR.decodeWord
        case tag of
            0 -> NodeInfo <$> CBOR.decode
                          <*> CBOR.decode
            _ ->
                fail "Error decoding NodeInfo: unknown tag"

mkNodeInfo :: Addr -> NodeId c -> NodeInfo c
mkNodeInfo = NodeInfo

-- | A host address either as an IP address or 'Hostname'
data Host = NumericHost IP | NamedHost Hostname
    deriving (Eq, Ord, Show, Generic)

instance Serialise Host where
    encode h =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encode (renderHost h)
    decode = do
        CBOR.decodeListLenOf 2
        tag <- CBOR.decodeWord
        case tag of
            0 -> do
                stringyHost <- CBOR.decode
                case readHost stringyHost of
                  Left e  -> fail ("Error decoding Host: " <> e)
                  Right h -> pure h
            _ ->
                fail "Error decoding Host: unknown tag"

numericHost :: IP -> Host
numericHost = NumericHost

namedHost :: Hostname -> Host
namedHost = NamedHost

renderHost :: Host -> Text
renderHost = either show renderHostname . hostToEither

readHost :: String -> Either String Host
readHost s =
        (NumericHost <$> readEither s)
    <|> (NamedHost   <$> readHostnameText (T.pack s))

-- | Isomorphism between 'Host' and 'Either IP Hostname'
hostEither
    :: (Profunctor p, Functor f)
    => p (Either IP Hostname) (f (Either IP Hostname))
    -> p Host                 (f Host)
hostEither = dimap hostToEither (eitherToHost <$>)
{-# INLINE hostEither #-}

hostToEither :: Host -> Either IP Hostname
hostToEither = \case
    NumericHost ip -> Left  ip
    NamedHost   hn -> Right hn

eitherToHost :: Either IP Hostname -> Host
eitherToHost = either NumericHost NamedHost

hostToHostName :: Host -> Network.HostName
hostToHostName = \case
    NumericHost ip -> show ip
    NamedHost   hn -> toS $ hostnameToDomain hn


-- | A host- or domain name.
--
-- It is guaranteed (via 'readHostnameText') that this is /not/ an IP address,
-- and that the name conforms to RFC 1123. Note that this implies that IDNs
-- would need to be passed punycode-encoded to 'readHostnameText'.
--
newtype Hostname = Hostname (Vector Text)
    deriving (Eq, Ord, Show, Generic)

instance Serialise Hostname

renderHostname :: Hostname -> Text
renderHostname (Hostname labels) =
      foldr' (mappend . toS) mempty
    . intersperse (T.singleton '.')
    $ toList labels

readHostnameText :: Text -> Either String Hostname
readHostnameText t
  | T.length t > 253 = Left "Hostname longer than 253 characters"
  | otherwise        = Hostname <$> foldrM label mempty
      (T.split (== '.') . T.dropWhileEnd (== '.') $ t)
  where
    label l !acc
      | T.length l > 63             = Left "Label longer than 63 chacters"
      | T.length l < 1              = Left "Label shorter than 1 character"
      | not (isAlphaNum (T.last l)) = Left "Last character in label must be alphanumeric"
      | Just c <- T.find (\c -> not (c == '-' || isAlphaNum c)) l =
        Left $ "Invalid character in label: " <> [c]
      | otherwise                   = Right (V.singleton l <> acc)

hostnameToDomain :: Hostname -> DNS.Domain
hostnameToDomain (Hostname labels) =
      foldr' (\l -> (toS l <>)) "."
    . intersperse (T.singleton '.')
    $ toList labels

domainToHostname :: DNS.Domain -> Either String Hostname
domainToHostname = readHostnameText . toS


-- | 'BootstrapInfo' of the current node.
--
-- Must include the 'NodeId' and HTTP API 'Addr'.
type SelfInfo = BootstrapInfo Identity

-- | 'BootstrapInfo' of a seed node discovered via "Oscoin.P2P.Disco".
--
-- Must include the gossip 'Addr', and may include the 'NodeId' and
-- HTTP API 'Addr'.
type SeedInfo = BootstrapInfo Maybe

data BootstrapInfo f c = BootstrapInfo
    { bootNodeId      :: f (NodeInfo c)
    , bootHttpApiAddr :: f Addr
    , bootGossipAddr  ::   Addr
    }

deriving instance ( Eq (f (NodeInfo c))
                  , Eq (f Addr)
                  ) => Eq   (BootstrapInfo f c)
deriving instance ( Show (f (NodeInfo c))
                  , Show (f Addr)
                  ) => Show (BootstrapInfo f c)

-- | Read a 'BootstrapInfo Maybe' from a 'String'. For use in CLI parsers.
--
-- The 'NodeId' is always 'Nothing'.
--
-- The input string is expected to be a @:@-separated pair of host : port, where
-- host may be an IP address or hostname. If host is an IPv6 address, it must be
-- enclosed in square brackets as per <https://tools.ietf.org/html/rfc3986#section-3.2.2 RFC 3986, Section 3.2.2>
-- in order to delimit it from the port number.
--
readBootstrapInfo :: String -> Either String (BootstrapInfo Maybe c)
readBootstrapInfo = \case
    ('[' : more) ->
        case break (== ']') more of
            (xs, ']' : ':' : rest) -> go xs rest
            _                      -> Left "Unmatched '[' when reading IPv6"
    xs           -> uncurry go . second (dropWhile (== ':')) $ break (== ':') xs
  where
    go :: String -> String -> Either String (BootstrapInfo Maybe c)
    go host port = do
        h <- readHost host
        p <- note "Invalid port number" (readMaybe port)
        pure $ BootstrapInfo Nothing Nothing (Addr h p)

-- | Show the 'NodeAddr' suitable for consumption by 'readNodeAddr'.
showBoostrapInfo :: BootstrapInfo Maybe c -> String
showBoostrapInfo BootstrapInfo { bootGossipAddr } =
    toS $ case addrHost bootGossipAddr of
        NumericHost IPv6{} -> "[" <> renderHost (addrHost bootGossipAddr) <> "]:" <> show nodePort
        _                  -> renderHost nodeHost <> ":" <> show nodePort

data Msg c tx s =
      BlockMsg (Block c tx s)
    | TxMsg    tx

deriving instance (Show (Hash c), Show tx, Show s) => Show (Msg c tx s)
deriving instance (Eq   (Hash c), Eq   tx, Eq   s) => Eq   (Msg c tx s)

instance
    ( Serialise (Block c tx s)
    , Serialise tx
    , Serialise s
    )
    => Serialise (Msg c tx s)
  where
    encode (BlockMsg blk) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encode blk

    encode (TxMsg tx) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 1
        <> CBOR.encode tx

    decode = do
        tag <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case tag of
            (2, 0) -> BlockMsg <$> CBOR.decode
            (2, 1) -> TxMsg    <$> CBOR.decode
            (2, _) -> fail "Oscoin.P2P.Types: Unknown tag for `Msg`"
            (_, _) -> fail "Oscoin.P2P.Types: Invalid listLen for `Msg`"

data MsgId c tx =
      BlockId (BlockHash c)
    | TxId    (Hashed c tx)

deriving instance Show (BlockHash c) => Show (MsgId c tx)
deriving instance Eq   (BlockHash c) => Eq   (MsgId c tx)

instance
    ( Serialise (BlockHash c)
    , Serialise tx
    )
    => Serialise (MsgId c tx)
  where
    encode (BlockId blkhsh) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encode blkhsh

    encode (TxId txhsh) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 1
        <> CBOR.encode txhsh

    decode = do
        tag <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case tag of
            (2, 0) -> BlockId <$> CBOR.decode
            (2, 1) -> TxId    <$> CBOR.decode
            (2, _) -> fail "Oscoin.P2P.Types: Unknown tag for `MsgId`"
            (_, _) -> fail "Oscoin.P2P.Types: Invalid listLen for `MsgId`"

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
