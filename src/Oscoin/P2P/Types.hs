module Oscoin.P2P.Types
    ( NodeId
    , mkNodeId

    , NodeAddr(..)

    , Msg(..)
    , MsgId(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)
import           Oscoin.Crypto.Hash (Hashed, fromHashed)
import           Oscoin.Crypto.PubKey (PublicKey, publicKeyHash)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Control.Monad.Fail (fail)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import           Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:))
import qualified Data.ByteArray as ByteArray
import           Data.Hashable (Hashable(..))
import           Network.Socket (HostName, PortNumber)

newtype NodeId = NodeId { fromNodeId :: Hashed ECDSA.PublicKey }
    deriving (Eq, Ord, Show, FromJSON, ToJSON)

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
            (2, 0) -> NodeId <$> CBOR.decode
            _      -> fail "CBOR NodeId: invalid tag"

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

data Msg tx =
      BlockMsg (Block tx ())
    | TxMsg    tx
    deriving (Eq, Generic)

instance Serialise tx => Serialise (Msg tx)

data MsgId tx =
      BlockId BlockHash
    | TxId    (Hashed tx)
    deriving (Eq, Generic)

instance Serialise tx => Serialise (MsgId tx)
