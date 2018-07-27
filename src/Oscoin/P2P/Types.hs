module Oscoin.P2P.Types
    ( NodeId (..)
    , Endpoints (..)
    , EndpointMap
    , NodeAddr (..)
    , Seed (..)
    , toSockAddr
    , fromSockAddr
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Hash (Hashed)

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Data.Binary (Binary(..))
import           Data.IP (IP(..))
import qualified Data.IP as IP
import           Data.Yaml (FromJSON, ToJSON, parseJSON, withObject, (.:))
import           Network.Socket (SockAddr(..))
import           Text.Read (readMaybe)

newtype NodeId = NodeId { fromNodeId :: Hashed ECDSA.PublicKey }
    deriving (Eq, Ord, Show, Binary, FromJSON, ToJSON)

data NodeAddr = NodeAddr
    { addrIP   :: IP
    , addrPort :: Word16
    } deriving (Eq, Show, Generic)

instance FromJSON NodeAddr where
    parseJSON = withObject "NodeAddr" $ \o -> do
        addrIP   <- read <$> o .: "ip"
        addrPort <-          o .: "port"
        pure NodeAddr{..}

instance Binary NodeAddr where
    put NodeAddr{addrIP, addrPort} = do
        put $ show addrIP
        put addrPort

    get = do
        addrIP   <- get >>= maybe (fail "Oscoin.P2P.Types.NodeAddr: Invalid IP") pure . readMaybe
        addrPort <- get
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

