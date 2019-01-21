{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Environment
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Environment

import           Test.Tasty
import           Test.Tasty.QuickCheck

instance Arbitrary Environment where
    arbitrary = arbitraryBoundedEnum


tests :: [TestTree]
tests =
    [ testProperty "toText . fromText == id" roundtripProperty
    ]

-- We don't need the full 100 tests here, 10 is enough provided
-- the underlying generator even distributes the test data (it does).
roundtripProperty :: Environment -> Property
roundtripProperty e = withMaxSuccess 10 $
    classify (e == Testing) "Testing" $
    classify (e == Development) "Development" $
    classify (e == Production) "Production"  $
        (fromText . toText $ e) === Just e
