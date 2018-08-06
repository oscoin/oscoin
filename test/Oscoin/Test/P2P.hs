module Oscoin.Test.P2P (tests) where

import qualified Oscoin.Test.P2P.Gossip.Membership as Membership

import           Test.Tasty

tests :: [TestTree]
tests =
    [ testGroup "Gossip" [Membership.tests]
    ]

