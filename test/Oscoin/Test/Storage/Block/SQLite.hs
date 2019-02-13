module Oscoin.Test.Storage.Block.SQLite
    ( genGenesisLinkedBlock
    , defaultGenesis
    , DummySeal
    , withSqliteDB
    , withMemStore
    , insertOrphans
    , bestChainOracle
    , shuffledOrphanChains
    , Shuffled(..)
    ) where

import           Oscoin.Prelude
import qualified Prelude

import           Codec.Serialise
import qualified Data.Sequence as Seq

import           Oscoin.Crypto.Blockchain hiding (genesisBlock)
import           Oscoin.Data.RadicleTx
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.Orphanage
import           Oscoin.Storage.Block.SQLite as Sqlite
import           Oscoin.Storage.Block.SQLite.Internal as Sqlite
import qualified Oscoin.Time as Time

import           Oscoin.Test.Crypto.Blockchain.Arbitrary (arbitraryBlock)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.Storage.Block.Generators

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

type DummySeal = Text

-- | Generates a genesis block with a slightly more realistic 'Difficulty'.
defaultGenesis :: Block tx DummySeal
defaultGenesis = sealBlock mempty (emptyGenesisBlock Time.epoch)

-- | Generates an arbitrary 'Block' which is linked with the genesis block
-- and that has a valid timestamp (> timestamp(genesis)).
genGenesisLinkedBlock :: Block RadTx DummySeal -> Gen (Block RadTx DummySeal)
genGenesisLinkedBlock genesisBlock =
    let genesisTimestamp = blockTimestamp . blockHeader $ genesisBlock
        blockInTheFuture = arbitraryBlock
            `suchThat` ((> genesisTimestamp) . blockTimestamp . blockHeader)
    in linkParent genesisBlock <$> blockInTheFuture

{------------------------------------------------------------------------------
  Useful combinators & helpers
------------------------------------------------------------------------------}

-- | Bracket-style initialiser for a SQLite DB.
withSqliteDB :: Show a
             => (Block RadTx DummySeal -> Gen a)
             -> (a -> Sqlite.Handle RadTx DummySeal -> IO ())
             -> Property
withSqliteDB genTestData action = once $ monadicIO $ do
    testData <- pick (genTestData defaultGenesis)
    liftIO $
        bracket (open ":memory:" blockScore blockValidate >>= initialize defaultGenesis)
                close
                (action testData)
  where
    blockValidate _ _ = Right ()

-- | Bracket-style initialiser for a SQLite-backed 'BlockStore'.
withMemStore :: Show a
             => (Block RadTx DummySeal -> Gen a)
             -> (a -> Abstract.BlockStore RadTx DummySeal IO -> IO ())
             -> Property
withMemStore genTestData action = once $ monadicIO $ do
    testData <- pick (genTestData defaultGenesis)
    liftIO $
        Sqlite.withBlockStore ":memory:" defaultGenesis blockScore blockValidate $ action testData
  where
    blockValidate _ _ = Right ()

-- | Inserts all the given blocks in the 'Orphanage'.
insertOrphans :: [Block tx s] -> Orphanage tx s -> Orphanage tx s
insertOrphans xs o =
    foldl' (flip insertOrphan) o xs

-- | Iterate over all the input chains and select the one with the best score.
bestChainOracle :: BlockHash
                -- ^ The hash of the adopted block this chain originates from.
                -> [(Shuffled (Blockchain tx s), Blockchain tx s, Block tx s)]
                -> Maybe (ChainCandidate s)
bestChainOracle _ [] = Nothing
bestChainOracle root xs =
    let (_shuffledChain, chain, lnk) =
            maximumBy (\(_,chain1,lnk1) (_,chain2, lnk2) ->
                sum (map blockScore (lnk1 : blocks chain1)) `compare`
                sum (map blockScore (lnk2 : blocks chain2))
            ) (filter (\(_,_,ls) -> parentHash ls == root) xs)
        winningChain = reverse (blocks chain <> [lnk])
    in Just $ ChainCandidate
        { candidateChain     = Seq.fromList (map blockHash winningChain)
        , candidateTipHeader = blockHeader (Prelude.last winningChain)
        , candidateScore     = sum (map blockScore winningChain)
        }

newtype Shuffled a = Shuffled { fromShuffled :: a }

-- | Returns a list of /shuffled/ orphans chains and, for conveniency, also
-- the original, ordered one.
shuffledOrphanChains :: (Arbitrary tx, Arbitrary s, Serialise tx, Serialise s)
                     => ForkParams
                     -> Blockchain tx s
                     -> Gen [(Shuffled (Blockchain tx s), Blockchain tx s, Block tx s)]
shuffledOrphanChains params initialChain = do
    xs <- genOrphanChainsFrom params initialChain
    forM xs (\(c,b) -> (,c,b) . Shuffled . unsafeToBlockchain <$> shuffle (blocks c))