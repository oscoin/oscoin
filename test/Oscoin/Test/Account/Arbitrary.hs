{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Account.Arbitrary where

import Oscoin.Prelude
import Oscoin.Account
import Oscoin.State.Tree (Key, Val)
import Oscoin.Account.Transaction

import Test.QuickCheck
import Test.QuickCheck.Instances ()

instance Arbitrary Tx where
    arbitrary = do
        accId  <- arbitrary :: Gen AccId
        accKey <- arbitrary :: Gen Key
        accVal <- arbitrary :: Gen Val
        pure $ setTx accId accKey accVal
