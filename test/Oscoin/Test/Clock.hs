{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oscoin.Test.Clock where

import           Oscoin.Clock
import           Oscoin.Prelude

import           Test.QuickCheck (Arbitrary)
import           Test.QuickCheck.Instances ()
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

deriving instance Arbitrary Timestamp

tests :: [TestTree]
tests =
    [ testGroup "Duration"
        [ testCase "1234 milliseconds as 1.234 seconds" $
            (1234 * milliseconds `as` seconds) @?= 1.234
        , testCase "3.5 hours as 210 minutes" $
            (3.5 * hours `as` minutes) @?= 210.0
        , testCase "1 second as 1000000 microseconds" $
            (1 * seconds `as` microseconds) @?= 1000000
        ]
    , testGroup "Timestamp"
        [ testCase "now" $ do
            before <- now
            after  <- now
            before < after @?= True
        ]
    ]
