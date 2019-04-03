module Oscoin.Tests
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import qualified Oscoin.Consensus.Config as Consensus
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Event as Mempool

import qualified Oscoin.Test.API as API
import qualified Oscoin.Test.API.HTTP as HTTP
import qualified Oscoin.Test.CLI as CLI
import qualified Oscoin.Test.Consensus as Consensus
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain (testBlockchain)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import qualified Oscoin.Test.Environment as Environment
import qualified Oscoin.Test.Storage.Block.Cache as BlockCache
import qualified Oscoin.Test.Storage.Block.Equivalence as StorageEquivalence
import qualified Oscoin.Test.Storage.Block.Orphanage as Orphanage
import qualified Oscoin.Test.Storage.Block.SQLite.Blackbox as SQLite.Blackbox
import qualified Oscoin.Test.Storage.Block.SQLite.Whitebox as SQLite.Whitebox
import qualified Oscoin.Test.Telemetry as Telemetry
import qualified Test.Oscoin.P2P as P2P

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck hiding ((===))

import           Data.ByteArray.Orphans ()

tests :: forall c. Dict (IsCrypto c) -> Consensus.Config -> TestTree
tests d@Dict config = testGroup "Oscoin"
    [ testGroup      "API.HTTP"                       (HTTP.tests d)
    -- ^ Testing HTTP API constructing HTTP requests manually
    , testGroup      "API"                            (API.tests d)
    -- ^ Testing API and Node using a 'MonadClient' instance
    , testGroup      "CLI"                            CLI.tests
    , testProperty   "Mempool"                        (testOscoinMempool d)
    , Consensus.tests d config
    , testGroup      "P2P"                            (P2P.tests d)
    , testGroup      "Storage Cache"                  (BlockCache.tests d)
    , testGroup      "Storage Orphanage"              (Orphanage.tests d)
    , testGroup      "Storage equivalence checking"   (StorageEquivalence.tests d)
    , testGroup      "SQLite Storage blackbox"        (SQLite.Blackbox.tests d)
    , testGroup      "SQLite Storage whitebox"        (SQLite.Whitebox.tests d)
    , testBlockchain d config
    , testGroup      "Telemetry"                      (Telemetry.tests d)
    , testGroup      "Environment"                    Environment.tests
    ]

testOscoinMempool :: forall c. Dict (IsCrypto c) -> Property
testOscoinMempool Dict = monadicIO $ do
    -- Create a new mempool of account transactions.
    mp <- lift (Mempool.newIO @c)

    -- Create some arbitrary transactions.
    txs <- pick (arbitrary @[API.RadTx c])

    -- Subscribe to the mempool with the subscription tokens.
    (chan1, chan2, evs1, evs2) <- lift $ atomically $ do
        chan1 <- Mempool.subscribe mp
        chan2 <- Mempool.subscribe mp

        -- Add the transactions.
        Mempool.insertMany mp txs

        -- Retrieve all events from the channels.
        evs1 <- Mempool.drainChannel chan1
        evs2 <- Mempool.drainChannel chan2

        pure (chan1, chan2, evs1, evs2)

    liftIO $ do
        -- Verify that they contain the transactions we generated.
        assertEqual "Chan 1 has all Txs" txs $ concat . mapMaybe fromEvent $ evs1
        assertEqual "Chan 2 has all Txs" txs $ concat . mapMaybe fromEvent $ evs2

        -- If we try to read again, the channel is empty.
        atomically (Mempool.drainChannel chan1) >>= assertEqual "Chan 1 is empty" []
        atomically (Mempool.drainChannel chan2) >>= assertEqual "Chan 2 is empty" []
  where
    fromEvent :: Mempool.Event tx -> Maybe [tx]
    fromEvent (Mempool.Insert txs) = Just txs
    fromEvent _                    = Nothing
