module Main (main) where

import           Oscoin.Prelude

import           Oscoin.Environment (Environment(Testing))
import           Oscoin.ProtocolConfig (getProtocolConfig)

import qualified Control.Concurrent.Tests as Concurrent
import qualified Crypto.Test.Hash.Multi as Multihash
import qualified Data.Conduit.Tests as Conduit
import qualified Oscoin.Tests as Oscoin
import           Test.Tasty

main :: IO ()
main = do
    protocolConfig <- getProtocolConfig Testing
    defaultMain $ testGroup "All"
        [ Oscoin.tests protocolConfig
        , Multihash.tests
        , Concurrent.tests
        , Conduit.tests
        ]
