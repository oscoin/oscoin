{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Serialise.Orphans where

import           GHC.Generics.Orphans ()

import           Codec.Serialise (Serialise)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECC

instance Serialise ECDSA.PublicKey
instance Serialise ECC.Curve
instance Serialise ECC.Point
instance Serialise ECC.CurveBinary
instance Serialise ECC.CurvePrime
instance Serialise ECC.CurveCommon
