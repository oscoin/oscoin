module Oscoin.Account.Arbitrary where

import Oscoin.Prelude
import Oscoin.Account
import Oscoin.Account.Transaction

import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary Tx where
    arbitrary = do
        accId  <- arbitrary :: Gen AccId
        accKey <- arbitrary :: Gen AccKey
        accVal <- arbitrary :: Gen AccVal
        pure $ setTx accId accKey accVal
