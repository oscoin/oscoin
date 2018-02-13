module Oscoin.Org.Arbitrary where

import Oscoin.Prelude
import Oscoin.Org
import Oscoin.Org.Transaction

import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary Tx where
    arbitrary = do
        orgId  <- arbitrary :: Gen OrgId
        orgKey <- arbitrary :: Gen OrgKey
        orgVal <- arbitrary :: Gen OrgVal
        pure $ setTx orgId orgKey orgVal
