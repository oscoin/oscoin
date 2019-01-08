module Oscoin.P2P.Types
    ( NodeId
    , mkNodeId
    , fromNodeId

    , NodeAddr(..)

    , Msg(..)
    , MsgId(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (Block, BlockHash)
import           Oscoin.Crypto.Hash (Hashed)
import           Oscoin.Crypto.PubKey (PublicKey)

import           Codec.Serialise (Serialise)
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
