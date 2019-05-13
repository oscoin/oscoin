module Oscoin.Test.Crypto.Blockchain.Block.Helpers where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (Beneficiary)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.QuickCheck

-- | Generates an arbitrary beneficiary.
defaultBeneficiary
    :: Arbitrary (Beneficiary c) => Beneficiary c
defaultBeneficiary = unsafePerformIO $ generate arbitrary
{-# NOINLINE defaultBeneficiary #-}
