module Oscoin.Test.Storage.Block.Cache
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block hiding (genesisBlock)
import           Oscoin.Time.Chrono (NewestFirst(..))

import           Oscoin.Storage.Block.Cache
                 (cached, consBlock, newBlockCache, viewBlocks)
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary ()

import           Data.ByteArray.Orphans ()

import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> [TestTree]
tests Dict =
    [ testGroup "Storage.Block.Cache"
        [ testProperty "works as a sequence"    (worksSequenceProp @c Dict)
        , testProperty "works as a ring buffer" (worksRingBufferProp @c Dict)
        , testProperty "cached works"           (cachedWorksProp @c Dict)
        ]
    ]

{------------------------------------------------------------------------------
  The tests proper
------------------------------------------------------------------------------}

worksSequenceProp :: forall c. Dict (IsCrypto c) -> Property
worksSequenceProp Dict = monadicIO $ do
    cacheSize <- pick (arbitrary `suchThat` (\x -> fromIntegral x > (0 :: Int)))
    cache <- liftIO (newBlockCache cacheSize)
    forAllM (arbitrary `suchThat` ((< fromIntegral cacheSize) . length)) $
        \(blks :: [Block c Text Text]) -> do
            storedBlocks <- liftIO $ do
                forM_ (reverse blks) (consBlock cache)
                viewBlocks cache
            assert $ storedBlocks == blks

worksRingBufferProp :: forall c. Dict (IsCrypto c) -> Property
worksRingBufferProp Dict = monadicIO $ do
    cacheSize <- pick (arbitrary `suchThat` (\x -> fromIntegral x > (0 :: Int)))
    cache <- liftIO (newBlockCache cacheSize)
    forAllM (arbitrary `suchThat` ((> fromIntegral cacheSize) . length)) $
        \(blks :: [Block c Text Text]) -> do
            storedBlocks <- liftIO $ do
                forM_ (reverse blks) (consBlock cache)
                viewBlocks cache
            assert $ storedBlocks == take (fromIntegral cacheSize) blks

-- | In this slightly convoluted 'Property', we generate two sets of data:
-- I.  A list of block which length is < of the input size;
-- II. A list of blocks which length is the \"backfill\" size.
--
-- After doing that, we request the 'BlockCache' exactly 'size' blocks, and
-- we expect the 'cached' function to lookup from disk correctly.
cachedWorksProp :: forall c. Dict (IsCrypto c) -> Positive Int -> Property
cachedWorksProp Dict (getPositive -> size) = monadicIO $ do
    cache <- liftIO (newBlockCache (fromIntegral size))
    let lessThanSize = arbitrary `suchThat` ((< size) . length)
        backfill bs  =
            NewestFirst <$>
            arbitrary `suchThat` (\b -> length b == size - length bs)

    forAllM lessThanSize $ \(blks :: [Block c Text Text]) ->
        forAllM (backfill blks) $ \backfilled -> do
            -- We first ask < size elements, and we expect the cache to return
            -- them.
            blocksFromCache <- liftIO $ do
                forM_ (reverse blks) (consBlock cache)
                cached cache (length blks) (const (pure backfilled))
            assert $ length blocksFromCache == length blks
            assert $ blocksFromCache == NewestFirst blks

            -- Then, we test that if the cache doesn't have enough blocks,
            -- we hit the disk.
            blocksFromDisk <- liftIO $
                cached cache size (const (pure backfilled))
            assert $ length blocksFromDisk == length backfilled
            assert $ blocksFromDisk == backfilled
