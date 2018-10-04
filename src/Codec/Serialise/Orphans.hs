{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Serialise.Orphans where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECC
import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteArray as ByteArray
import           Data.Tagged (Tagged(..))

instance Serialise b => Serialise (Tagged s b)

instance ByteArrayAccess b => ByteArrayAccess (Tagged s b) where
    length                    = ByteArray.length . unTagged
    withByteArray (Tagged ba) = withByteArray ba

deriving instance Generic ECDSA.PublicKey
instance Serialise ECDSA.PublicKey

deriving instance Generic ECC.Curve
instance Serialise ECC.Curve

deriving instance Generic ECC.Point
instance Serialise ECC.Point

deriving instance Generic ECC.CurveBinary
instance Serialise ECC.CurveBinary

deriving instance Generic ECC.CurvePrime
instance Serialise ECC.CurvePrime

deriving instance Generic ECC.CurveCommon
instance Serialise ECC.CurveCommon

