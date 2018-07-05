module Oscoin.P2P.Types
    ( NodeId (..)
    , Endpoints (..)
    , NodeAddr (..)
    , toSockAddr
    , fromSockAddr
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.PubKey (PublicKey)

import           Data.Binary (Binary(..))
import           Data.IP (IP(..))
import qualified Data.IP as IP
import           Network.Socket (SockAddr(..))
import           Text.Read (readMaybe)

newtype NodeId = NodeId { fromNodeId :: PublicKey }
    deriving (Eq, Ord, Show, Binary)

data NodeAddr = NodeAddr
    { addrIP   :: IP
    , addrPort :: Word16
    } deriving (Eq, Show)

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
