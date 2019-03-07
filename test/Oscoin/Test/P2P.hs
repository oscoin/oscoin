module Oscoin.Test.P2P (tests, props) where

import           Oscoin.Prelude

import           Oscoin.Test.Crypto
import qualified Oscoin.Test.P2P.Disco as Disco
import qualified Oscoin.Test.P2P.Handshake as Handshake
import qualified Oscoin.Test.P2P.IO as IO
import qualified Oscoin.Test.P2P.Transport as Transport
import qualified Oscoin.Test.P2P.Types as Types

import           Test.Tasty

tests :: Dict (IsCrypto c) -> [TestTree]
tests d =
    [ Disco.tests
    , Handshake.tests d
    , IO.tests
    , Transport.tests
    , Types.tests d
    ]

props :: Dict (IsCrypto c) -> IO Bool
props d = and <$> sequence
    [ Disco.props
    , Handshake.props d
    , IO.props
    , Transport.props
    , Types.props d
    ]
