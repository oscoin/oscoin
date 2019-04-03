module Oscoin.Tests
    ( tests
    ) where

import qualified Oscoin.Consensus.Config as Consensus

import qualified Oscoin.Test.API.HTTP as HTTP
import qualified Oscoin.Test.CLI as CLI
import qualified Oscoin.Test.Consensus as Consensus
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain (testBlockchain)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import qualified Oscoin.Test.Storage.Block.Cache as BlockCache
import qualified Oscoin.Test.Storage.Block.Equivalence as StorageEquivalence
import qualified Oscoin.Test.Storage.Block.SQLite.Blackbox as SQLite.Blackbox
import qualified Oscoin.Test.Storage.Block.SQLite.Whitebox as SQLite.Whitebox
import qualified Oscoin.Test.Telemetry as Telemetry
import qualified Test.Oscoin.Crypto.Address as Address
import qualified Test.Oscoin.P2P as P2P

import           Test.QuickCheck.Instances ()
import           Test.Tasty

import           Data.ByteArray.Orphans ()

tests :: forall c. Dict (IsCrypto c) -> Consensus.Config -> TestTree
tests d@Dict config = testGroup "Oscoin"
    [ testGroup      "API.HTTP"                       (HTTP.tests d)
    -- ^ Testing HTTP API constructing HTTP requests manually
    , testGroup      "CLI"                            CLI.tests
    , Consensus.tests d config
    , testGroup      "Address"                        (Address.tests d)
    , testGroup      "P2P"                            (P2P.tests d)
    , testGroup      "Storage Cache"                  (BlockCache.tests d)
    , testGroup      "Storage equivalence checking"   (StorageEquivalence.tests d)
    , testGroup      "SQLite Storage blackbox"        (SQLite.Blackbox.tests d)
    , testGroup      "SQLite Storage whitebox"        (SQLite.Whitebox.tests d)
    , testBlockchain d config
    , testGroup      "Telemetry"                      (Telemetry.tests d)
    ]
