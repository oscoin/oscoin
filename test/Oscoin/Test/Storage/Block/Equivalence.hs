{- | In this module we spawn two different 'BlockStore' implementations, the
   pure and the SQLite one and we check their operations are equivalent, i.e
   both implementations returns the same results.
-}
module Oscoin.Test.Storage.Block.Equivalence
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Nakamoto (blockScore)
import           Oscoin.Crypto.Blockchain
import           Oscoin.Protocol
import           Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.Orphanage
import qualified Oscoin.Storage.Block.SQLite as SQLite
import qualified Oscoin.Storage.Block.STM as STM
import qualified Oscoin.Time.Chrono as Chrono

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Generators
                 (ForkParams(..), genBlockchainFrom, genOrphanChainsFrom)
import           Oscoin.Test.Storage.Block.SQLite (DummySeal, defaultGenesis)
import           Oscoin.Test.Util (Condensed(..))

import           Control.Monad.State (modify')
import           Data.ByteArray.Orphans ()
import qualified Data.List as List
import qualified Data.Text as T
import           GHC.Exception (srcLocFile, srcLocStartCol, srcLocStartLine)

import           Test.Oscoin.DummyLedger
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: Dict (IsCrypto c) -> [TestTree]
tests d =
    [ testProperty "getTip . insertBlock equivalence"  (propInsertGetTipEquivalence d)
    , testProperty "(forks) getTip . insertBlock equivalence"  (propForksInsertGetTipEquivalence d)
    ]

-- | Classify a 'Blockchain' based on its length.
classifyChain :: Blockchain c tx s -> Property -> Property
classifyChain chain =
    let chainLen = length (blocks chain)
    in classify (chainLen <= 10) "small chains  (< 10 blocks)" .
       classify (chainLen <= 50) "medium chains (< 50 blocks)" .
       classify (chainLen > 50)  "long chains   (> 50 blocks)"

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

-- | Check that given a random 'Blockchain', we can insert a bunch of blocks
-- and get the tip, and that both stores agree on the result.
propInsertGetTipEquivalence :: forall c.  Dict (IsCrypto c) -> Property
propInsertGetTipEquivalence Dict =
    forAllShrink (resize 25 $ genBlockchainFrom (defaultGenesis @c)) genericShrink $ \chain ->
        classifyChain chain $
            ioProperty $ withStores $ \stores -> do
                p1 <- privateApiCheck stores (`Abstract.insertBlocksNaive` (Chrono.reverse . blocks) chain)
                p2 <- publicApiCheck stores Abstract.getTip
                pure (p1 .&&. p2)

-- | In this 'Property', we do the following:
-- 1. We start from a default chain;
-- 2. We generate some random orphan chains together with the \"missing link\"
--    necessary to make them non-orphans;
-- 3. We insert the orphan chain.
-- 4. We add the \"missing link\", assessing the tip coincides.
propForksInsertGetTipEquivalence :: forall c. Dict (IsCrypto c) -> Property
propForksInsertGetTipEquivalence Dict = do
    let forkParams = ForkParams 0 10 3  -- 3 forks of max 10 blocks.
        orphanage  = emptyOrphanage blockScore
        generator = do
            chain <- resize 15 $ genBlockchainFrom (defaultGenesis @c)
            orph  <- genOrphanChainsFrom forkParams chain
            pure (chain, orph)
    forAll generator $ \(chain, orphansWithLink) ->
        ioProperty $ withStoresAndProto orphanage $ \stores -> do
            -- Step 1: Store the chain in both stores.
            p0 <- privateApiCheck stores (`Abstract.insertBlocksNaive` (Chrono.reverse . blocks) chain)
            ps <- forM orphansWithLink $ \(orphans, missingLink) -> do
                -- Step 2: Store the orphan chains
                p1 <- privateApiCheck stores (`Abstract.insertBlocksNaive` (Chrono.reverse . blocks) orphans)
                -- Step 3: Add the missing link and check the tip
                p2 <- privateApiCheck stores (`Abstract.insertBlocksNaive` Chrono.OldestFirst [missingLink])
                p3 <- publicApiCheck stores Abstract.getTip
                pure [p1,p2,p3]
            pure $ foldl' (.&&.) p0 (mconcat ps)

{------------------------------------------------------------------------------
  Useful combinators
------------------------------------------------------------------------------}

type StoresUnderTest c tx s m =
    (Abstract.BlockStore c tx s m, Abstract.BlockStore c tx s m)

-- | The monad whe stores runs in, which keeps around an 'Orphanage' to be
-- used internally for the SQLite store implementation (cfr. 'withStores').
type StoreM c = StateT (Orphanage c DummyTx DummySeal) IO


-- | Initialises both the SQL and the STM store and pass them to the 'action'.
-- For the SQL store, we need to cheat: due to the fact we /do not/ want to
-- run the 'Protocol' chain selection on both stores (or it would defy the
-- purpose of these tests) we need to override the 'insertBlock' for the
-- SQLite store to pass through chain selection.
withStores
    :: forall c a.
       IsCrypto c
    => (StoresUnderTest c DummyTx DummySeal IO -> IO a)
    -> IO a
withStores action =
    SQLite.withBlockStore ":memory:" defaultGenesis $ \sqlStore ->
        STM.withBlockStore (fromGenesis defaultGenesis) blockScore $ \stmStore ->
            action (sqlStore, stmStore)

withStoresAndProto
    :: forall c a.
       IsCrypto c
    => Orphanage c DummyTx DummySeal
    -> (StoresUnderTest c DummyTx DummySeal (StoreM c) -> StoreM c a)
    -> IO a
withStoresAndProto orphanage action =
    SQLite.withBlockStore ":memory:" defaultGenesis $ \sqlStore ->
        STM.withBlockStore (fromGenesis defaultGenesis) blockScore $ \stmStore ->
            evalStateT (action (wrapProto (hoistBlockStore liftIO sqlStore), hoistBlockStore liftIO stmStore)) orphanage
  where
      -- Overrides the 'insertBlock' to pass via chain selection.
      wrapProto
          :: BlockStore c DummyTx DummySeal (StoreM c)
          -> BlockStore c DummyTx DummySeal (StoreM c)
      wrapProto bs@(public, private) =
          let cfg = Consensus.Config 1024 2016
              noValidation _ _ = Right ()
          in (public, private { insertBlock = \b -> do
                                    o  <- get
                                    (p', _) <- withProtocol o noValidation blockScore bs cfg $ \p ->
                                                   stepProtocol p b
                                    modify' (const (protoOrphanage p'))
                              })

publicApiCheck
    :: forall c tx s m b.
       ( HasCallStack
       , Monad m
       , Eq b
       , Show b
       , Condensed b
       )
    => StoresUnderTest c tx s m
    -> (Abstract.BlockStoreReader c tx s m -> m b)
    -> m Property
publicApiCheck (store1, store2) apiCall =
    withFrozenCallStack $ do
        res1 <- apiCall (fst store1)
        res2 <- apiCall (fst store2)
        pure $ counterexample (apiMismatch callStack res1 res2) (res1 === res2)

privateApiCheck
    :: forall c tx s m b.
       ( HasCallStack
       , Monad m
       , Eq b
       , Show b
       , Condensed b
       )
    => StoresUnderTest c tx s m
    -> (Abstract.BlockStoreWriter c tx s m -> m b)
    -> m Property
privateApiCheck (store1, store2) apiCall =
    withFrozenCallStack $ do
        res1 <- apiCall (snd store1)
        res2 <- apiCall (snd store2)
        pure $ counterexample (apiMismatch callStack res1 res2) (res1 === res2)

-- | When given a function from a 'BlockStore' operation to a result 'a', it
-- calls the function over both stores and returns whether or not the result
-- matches.
apiMismatch
    :: Condensed b
    => CallStack
    -> b
    -> b
    -> String
apiMismatch cs res1 res2 =
    let calledAt = case getCallStack cs of
                     [(_, loc)] -> srcLocFile loc <> ":" <>
                                   show (srcLocStartLine loc) <> ":" <>
                                   show (srcLocStartCol loc)
                     _          -> "(unknown)"
    in List.unlines [
             "api call at " <> calledAt <> " yielded a result mismatch!\n"
           , "sqlStore  = " <> T.unpack (condensed res1) <> "\n"
           , "pureStore = " <> T.unpack (condensed res2) <> "\n"
           , "Counterexample is:"
           ]
