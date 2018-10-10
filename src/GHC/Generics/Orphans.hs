{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Generics.Orphans where

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECC
import           GHC.Generics (Generic)

deriving instance Generic ECDSA.PublicKey
deriving instance Generic ECC.Curve
deriving instance Generic ECC.Point
deriving instance Generic ECC.CurveBinary
deriving instance Generic ECC.CurvePrime
deriving instance Generic ECC.CurveCommon
