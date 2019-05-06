module Test.Oscoin.P2P (tests, props) where

import           Oscoin.Prelude

import           Oscoin.Test.Crypto

import qualified Test.Oscoin.P2P.Disco as Disco
import qualified Test.Oscoin.P2P.Disco.Options as Disco.Options
import qualified Test.Oscoin.P2P.Handshake as Handshake
import qualified Test.Oscoin.P2P.IO as IO
import qualified Test.Oscoin.P2P.Transport as Transport
import qualified Test.Oscoin.P2P.Types as Types

import           Test.Tasty

tests :: Dict (IsCrypto c) -> [TestTree]
tests d =
    [ Disco.tests
    , Disco.Options.tests d
    , Handshake.tests d
    , IO.tests
    , Transport.tests
    , Types.tests d
    ]

props :: Dict (IsCrypto c) -> IO Bool
props d = and <$> sequence
    [ Disco.props
    , Disco.Options.props d
    , Handshake.props d
    , IO.props
    , Transport.props
    , Types.props d
    ]
