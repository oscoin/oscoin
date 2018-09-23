{-# LANGUAGE TupleSections #-}

module Oscoin.P2P.Types
    ( NodeId
    , mkNodeId

    , Endpoints (..)
    , EndpointMap
    , NodeAddr (..)
    , Seed (..)

    , toSockAddr
    , fromSockAddr
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashed, fromHashed, toHashed)
import           Oscoin.Crypto.PubKey (PublicKey, publicKeyHash)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Monad.Fail (fail)
import           Crypto.Hash (digestFromByteString)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Data.Binary (Binary(..))
import qualified Data.Binary as Binary
import qualified Data.ByteArray as ByteArray
import           Data.Hashable (Hashable(..))
import           Data.IP (IP(..))
import qualified Data.IP as IP
import           Data.Yaml (FromJSON, ToJSON, parseJSON, withObject, (.:))
import           Network.Socket (SockAddr(..))
import           Text.Read (Read(..), readMaybe)

newtype NodeId = NodeId { fromNodeId :: Hashed ECDSA.PublicKey }
    deriving (Eq, Ord, Show, Binary, FromJSON, ToJSON)

mkNodeId :: PublicKey -> NodeId
mkNodeId = NodeId . publicKeyHash

instance Hashable NodeId where
    hashWithSalt salt (NodeId h) =
        let digest = fromHashed h
            bytes  = ByteArray.convert digest :: ByteString
         in hashWithSalt salt bytes

instance Serialise NodeId where
    encode (NodeId pk) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encodeBytes (ByteArray.convert pk)

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (2, 0) -> do
                bs <- CBOR.decodeBytes
                maybe (fail "CBOR NodeId: invalid digest") pure $
                    NodeId . toHashed <$> digestFromByteString bs

            _ -> fail "CBOR NodeId: invalid tag"

data NodeAddr = NodeAddr
    { addrIP   :: IP
    , addrPort :: Word16
    } deriving (Eq, Show, Generic)

instance FromJSON NodeAddr where
    parseJSON = withObject "NodeAddr" $ \o -> do
        ip       <- o .: "ip"
        addrIP   <- maybe (fail ("Invalid IP: " <> show ip)) pure $ readMaybe ip
        addrPort <- o .: "port"
        pure NodeAddr{..}

instance Read NodeAddr where
    readsPrec _ input = maybeToList . map (,"") $ do
        addrIP   <- readMaybe $ takeWhile (/= ':') input
        addrPort <- readMaybe . drop 1 . dropWhile (/= ':') $ input
        pure NodeAddr{..}

instance Binary NodeAddr where
    put NodeAddr{addrIP, addrPort} = do
        Binary.put @String $ show addrIP
        Binary.put addrPort

    get = do
        addrIP   <- Binary.get >>= maybe (fail "Oscoin.P2P.Types.NodeAddr: Invalid IP") pure . readMaybe
        addrPort <- Binary.get
        pure NodeAddr{..}

toSockAddr :: NodeAddr -> SockAddr
toSockAddr (NodeAddr (IPv4 ip) port) =
    SockAddrInet (fromIntegral port) (IP.toHostAddress ip)
toSockAddr (NodeAddr (IPv6 ip) port) =
    SockAddrInet6 (fromIntegral port) 0 (IP.toHostAddress6 ip) 0

fromSockAddr :: SockAddr -> Maybe NodeAddr
fromSockAddr (SockAddrInet port host) = pure $
    NodeAddr (IPv4 $ IP.fromHostAddress host) (fromIntegral port)
fromSockAddr (SockAddrInet6 port _ host _) = pure $
    NodeAddr (IPv6 $ IP.fromHostAddress6 host) (fromIntegral port)
fromSockAddr _ = Nothing

data Endpoints = Endpoints
    { apiEndpoint :: NodeAddr
    , p2pEndpoint :: NodeAddr
    } deriving (Show, Generic)

instance Binary Endpoints
instance FromJSON Endpoints

type EndpointMap = Map NodeId Endpoints

data Seed = Seed
    { seedId        :: NodeId
    , seedEndpoints :: Endpoints
    } deriving (Show, Generic)

instance FromJSON Seed

