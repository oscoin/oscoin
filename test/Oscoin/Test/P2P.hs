module Oscoin.Test.P2P (tests) where

import qualified Oscoin.Test.P2P.Handshake as Handshake
import qualified Oscoin.Test.P2P.IO as IO
import qualified Oscoin.Test.P2P.Transport as Transport

import           Test.Tasty

tests :: [TestTree]
tests =
    [ testGroup "Gossip"
        [ IO.tests
        , Handshake.tests
        , Transport.tests
        ]
    ]

