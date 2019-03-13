{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Crypto.PubKey.Internal
    ( PK(..)
    , SK(..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto

import           Codec.Serialise.Orphans ()


{------------------------------------------------------------------------------
 Internal Types -- Use only if you know what you are doing
------------------------------------------------------------------------------}

-- | A 'PK' over a certain crypto 'c' and an algorithm.
-- /DO NOT/ use this type directly, it's exported only for the sake of
-- implementing the 'HasDigitalSignature' instance in 'PubKey.RealWorld'.
-- Use the abstract PublicKey/PrivateKey instead.
data PK c k = PK k (Crypto.Hashed c k)

deriving instance (Show (Crypto.Hash c), Crypto.HasHashing c, Show k) => Show (PK c k)
deriving instance Generic (PK c k)

-- | A 'SK over a certain algorithm.
-- /DO NOT/ use this type directly, it's exported only for the sake of
-- implementing the 'HasDigitalSignature' instance in 'PubKey.RealWorld'.
-- Use the abstract PublicKey/PrivateKey instead.
newtype SK k = SK k
