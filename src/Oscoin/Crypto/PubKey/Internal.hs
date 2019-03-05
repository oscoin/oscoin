{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Crypto.PubKey.Internal
    ( PublicKey(..)
    , PrivateKey(..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto

import           Codec.Serialise.Orphans ()


{------------------------------------------------------------------------------
 Internal Types -- Use only if you know what you are doing
------------------------------------------------------------------------------}

-- | A 'PublicKey' over a certain crypto 'c' and an algorithm.
-- /DO NOT/ use this type directly, it's exported only for the sake of
-- implementing the 'HasDigitalSignature' instance in 'PubKey.RealWorld'.
-- Use the abstract PK/SK instead.
data PublicKey c k = PublicKey k (Crypto.Hashed c k)

deriving instance (Show (Crypto.Hash c), Crypto.HasHashing c, Show k) => Show (PublicKey c k)
deriving instance Generic (PublicKey c k)

-- | A 'PrivateKey over a certain algorithm.
-- /DO NOT/ use this type directly, it's exported only for the sake of
-- implementing the 'HasDigitalSignature' instance in 'PubKey.RealWorld'.
-- Use the abstract PK/SK instead.
newtype PrivateKey k = PrivateKey k
