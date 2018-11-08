module Network.Gossip.IO.Wire
    ( WireMessage (..)
    ) where

import           Prelude

import           Codec.Serialise (Serialise)
import           Data.Text (Text)
import           GHC.Generics (Generic)

data WireMessage p =
      WirePayload p
    | WireGoaway (Maybe Text)
    deriving (Generic)

instance Serialise p => Serialise (WireMessage p)
