module Main (main) where

import           Oscoin.Prelude

import qualified Control.Concurrent.Tests as Concurrent
import qualified Crypto.Test.Hash.Multi as Multihash
import qualified Data.Conduit.Tests as Conduit
import qualified Network.Gossip.Test.Broadcast as Broadcast
import qualified Network.Gossip.Test.Membership as Membership
import qualified Oscoin.Tests as Oscoin
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All"
    [ Oscoin.tests
    , Multihash.tests
    , Concurrent.tests
    , Conduit.tests
    , Broadcast.tests
    , Membership.tests
    ]
