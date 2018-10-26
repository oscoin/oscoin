module Network.Gossip.IO.Peer
    ( Peer (..)
    , knownPeer
    )
where

import           Prelude hiding (fail)

import           Codec.Serialise (Serialise(..))
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Applicative (liftA2)
import           Control.Monad.Fail (fail)
import           Data.Hashable (Hashable(..), hashUsing)
import           Data.Word (Word8)
import           GHC.Stack (HasCallStack)
import           Network.Socket
import           Network.Socket.Serialise (decodeSockAddr, encodeSockAddr)

data Peer n = Peer
    { peerNodeId :: n
    , peerAddr   :: SockAddr
    } deriving (Eq, Show)

instance Serialise n => Serialise (Peer n) where
    encode (Peer nid addr) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 0
        <> encode nid
        <> encodeSockAddr addr

    decode = do
        pre <- liftA2 (,) CBOR.decodeListLen CBOR.decodeWord
        case pre of
            (3, 0) -> liftA2 Peer decode decodeSockAddr
            _      -> fail "CBOR Peer: invalid tag"

instance Hashable n => Hashable (Peer n) where
    hashWithSalt salt (Peer nid addr) =
        (salt `hashWithSalt` nid) `hashAddr` addr
      where
        hashAddr s (SockAddrInet portNum hostAddr) =
            (s `hashWithSalt` (0 :: Word8))
               `hashPortNum`  portNum
               `hashWithSalt` hostAddr

        hashAddr s (SockAddrInet6 portNum flow hostAddr scope) =
            (s `hashWithSalt` (1 :: Word8))
               `hashPortNum`  portNum
               `hashWithSalt` flow
               `hashWithSalt` hostAddr
               `hashWithSalt` scope

        hashAddr s (SockAddrUnix path) =
            s `hashWithSalt` (2 :: Word8) `hashWithSalt` path

        -- hashAddr s (SockAddrCan x) = canNotSupported
        hashAddr _ _ = canNotSupported

        hashPortNum = hashUsing fromEnum

knownPeer :: n -> HostName -> PortNumber -> IO (Peer n)
knownPeer nid host port = Peer nid <$> resolve
  where
    resolve = do
        let hints = defaultHints
                      { addrFlags      = [AI_ALL, AI_NUMERICSERV]
                      , addrSocketType = Stream
                      }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just (show port))
        pure $ addrAddress addr

--------------------------------------------------------------------------------

canNotSupported :: HasCallStack => a
canNotSupported = error "CAN addresses not supported"
