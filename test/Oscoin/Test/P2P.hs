module Oscoin.Test.P2P (tests) where

import           Oscoin.Test.Crypto
import qualified Oscoin.Test.P2P.Handshake as Handshake
import qualified Oscoin.Test.P2P.IO as IO
import qualified Oscoin.Test.P2P.Transport as Transport

import           Test.Tasty

tests :: Dict (IsCrypto c) -> [TestTree]
tests d =
    [ testGroup "Gossip"
        [ IO.tests
        , Handshake.tests d
        , Transport.tests
        ]
    ]

