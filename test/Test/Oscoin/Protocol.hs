module Test.Oscoin.Protocol
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Nakamoto (blockScore)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Protocol
import           Oscoin.Storage.Block.Abstract as Abstract
import qualified Oscoin.Storage.Block.BlockTree.RealWorld as RealWorld
import qualified Oscoin.Storage.Block.BlockTree.Reference as Reference
import qualified Oscoin.Storage.Block.STM as STM
import qualified Oscoin.Time.Chrono as Chrono

import qualified Oscoin.Storage.Block.SQLite as SQLite
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Generators
                 (ForkParams(..), genBlockchainFrom, genOrphanChainsFrom)
import           Oscoin.Test.Storage.Block.SQLite (DummySeal, defaultGenesis)
import           Oscoin.Test.Util (Condensed(..))

import           Data.ByteArray.Orphans ()

import           Test.Oscoin.DummyLedger
import           Test.QuickCheck.Extended
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding ((===))

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Protocol"
    [ testProperty "prop_forkInsertGetTipEquivalence"  (prop_forkInsertGetTipEquivalence d)
    ]


-- | In this 'Property', we do the following:
-- 1. We start from a default chain;
-- 2. We generate some random orphan chains together with the \"missing link\"
--    necessary to make them non-orphans;
-- 3. We insert the orphan chain.
-- 4. We add the \"missing link\"
-- 5. We return the new tip and compare it bewteen the two
--    implementations.
prop_forkInsertGetTipEquivalence :: forall c. Dict (IsCrypto c) -> Property
prop_forkInsertGetTipEquivalence Dict = do
    let forkParams = ForkParams 0 10 3  -- 3 forks of max 10 blocks.
        generator = do
            chain <- genBlockchainFrom (defaultGenesis @c)
            orph  <- genOrphanChainsFrom forkParams chain
            pure (chain, orph)
    forAll generator $ \(chain, orphansWithLink) ->
        testEquivalence $ \blockStoreReader dispatchBlock -> do
            -- Step 1: Store the chain in both stores.
            traverse_ dispatchBlock $ Chrono.toOldestFirst $ Chrono.reverse $ blocks chain
            forM orphansWithLink $ \(orphans, missingLink) -> do
                -- Step 2: Store the orphan chains
                traverse_ dispatchBlock $ Chrono.toOldestFirst $ Chrono.reverse $ blocks orphans
                -- Step 3: Add the missing link and check the tip
                dispatchBlock missingLink
                Abstract.getTip blockStoreReader


type EquivalenceTestRunner c a =
    forall m. (Monad m)
    => Abstract.BlockStoreReader c DummyTx DummySeal m
    -> (Block c DummyTx (Sealed c DummySeal) -> m ())
    -> m a


-- | Test that the given 'EquivalenceTestRunner' produces the same
-- output when run against the protocol and the model.
testEquivalence
    :: forall c a. (IsCrypto c, Eq a, Show a, Condensed a)
    => EquivalenceTestRunner c a
    -> Property
testEquivalence runScript = monadicIO $ do
    metricsStore <- liftIO $ Telemetry.newMetricsStore Telemetry.noLabels
    let telemetry = Telemetry.newTelemetryStore Telemetry.noLogger metricsStore

    stmValue <- liftIO (runWithStm telemetry)
    sqlValue <- liftIO (runWithSqlite telemetry)
    pure $ sqlValue === stmValue
  where

    initialBlockchain = fromGenesis defaultGenesis
    cfg = Consensus.Config 1024 2016
    noValidation _ _ = Right ()

    runWithStm :: Telemetry.Handle -> IO a
    runWithStm telemetry =
        STM.withBlockStore initialBlockchain blockScore $ \blkStore -> do
            let btree = RealWorld.newBlockTree cfg noValidation blockScore blkStore
            runProtocol noValidation blockScore telemetry btree cfg $ \Handle{dispatchBlockSync} ->
                runScript (fst blkStore) dispatchBlockSync

    runWithSqlite :: Telemetry.Handle -> IO a
    runWithSqlite telemetry =
        SQLite.withBlockStore ":memory:" defaultGenesis $ \blkStore -> do
            let btree = Reference.newBlockTree blkStore
            runProtocol noValidation blockScore telemetry btree cfg $ \Handle{dispatchBlockSync} ->
                runScript (fst blkStore) dispatchBlockSync

