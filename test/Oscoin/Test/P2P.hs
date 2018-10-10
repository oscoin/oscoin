module Oscoin.Test.P2P (tests) where

import qualified Oscoin.Test.P2P.Gossip.Broadcast as Broadcast
import qualified Oscoin.Test.P2P.Gossip.IO as IO
import qualified Oscoin.Test.P2P.Gossip.Membership as Membership
import qualified Oscoin.Test.P2P.Handshake as Handshake
import qualified Oscoin.Test.P2P.Transport as Transport

import           Test.Tasty

tests :: [TestTree]
tests =
    [ testGroup "Gossip"
        [ Broadcast.tests
        , IO.tests
        , Handshake.tests
        , Transport.tests
        , Membership.tests
        ]
    ]

