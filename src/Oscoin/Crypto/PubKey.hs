{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Crypto.PubKey
    (
    -- * Signing and verifying messages
      HasDigitalSignature(..)
    , unsign

    , KeyPair

    -- * Internals
    , Signed(..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto

import           Crypto.Random.Types (MonadRandom)

import           Codec.Serialise (Serialise(..))
import           Codec.Serialise.Orphans ()
import           Data.Aeson
                 (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import           Data.ByteArray (ByteArrayAccess)
import           Data.Text.Prettyprint.Doc

{------------------------------------------------------------------------------
 Types
------------------------------------------------------------------------------}

type KeyPair c = (PK c, SK c)

data Signed c msg = Signed
    { sigMessage   :: msg
    , sigSignature :: Signature c
    } deriving (Functor, Generic, Foldable, Traversable)

deriving instance (Show (Signature c), Show msg) => Show (Signed c msg)
deriving instance (Eq (Signature c), Eq msg) => Eq (Signed c msg)
deriving instance (Ord (Signature c), Ord msg) => Ord (Signed c msg)

{------------------------------------------------------------------------------
 Digital signatures - signing and verifying things
------------------------------------------------------------------------------}

class HasDigitalSignature c where
    data family PK c :: *
    data family SK c :: *
    data family Signature c :: *

    sign   :: (ByteArrayAccess msg, MonadRandom m) => SK c -> msg -> m (Signed c msg)
    verify :: ByteArrayAccess msg => PK c -> Signed c msg -> Bool

    -- | Generate a new random keypair.
    generateKeyPair :: (Crypto.HasHashing c, MonadRandom m) => m (KeyPair c)


{------------------------------------------------------------------------------
  Other Instances
------------------------------------------------------------------------------}

instance Crypto.Hashable c msg => Crypto.Hashable c (Signed c msg) where
    hash :: Signed c msg -> Crypto.Hashed c (Signed c msg)
    hash (Signed msg _) = Crypto.toHashed (Crypto.fromHashed (Crypto.hash msg))

instance (ToJSON msg, ToJSON (Signature c)) => ToJSON (Signed c msg) where
    toJSON (Signed msg sig) =
        object [ "msg" .= toJSON msg
               , "sig" .= toJSON sig
               ]

instance (FromJSON msg, FromJSON (Signature c)) => FromJSON (Signed c msg) where
    parseJSON = withObject "Signed a" $ \o -> do
        msg <- o .: "msg"
        sig <- o .: "sig"
        pure $ Signed msg sig

instance (Serialise msg, Serialise (Signature c)) => Serialise (Signed c msg)

instance Pretty msg => Pretty (Signed c msg) where
    pretty = pretty . unsign

--------------------------------------------------------------------------------

-- | Extracts the message payload out of the 'Signed' opaque type.
unsign :: Signed c msg -> msg
unsign (Signed msg _) = msg
