module Oscoin.Test.Storage.Block.SQLite
    ( defaultGenesis
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

import           Oscoin.Consensus.Nakamoto (blockScore)
import           Oscoin.Crypto.Blockchain hiding (genesisBlock)
import           Oscoin.Data.RadicleTx
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.Orphanage
import           Oscoin.Storage.Block.SQLite as Sqlite
import           Oscoin.Storage.Block.SQLite.Internal as Sqlite
import qualified Oscoin.Time as Time

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Generators
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

type DummySeal = Text

-- | Generates a genesis block with a slightly more realistic 'Difficulty'.
defaultGenesis :: IsCrypto c => Block c tx (Sealed c DummySeal)
defaultGenesis = sealBlock mempty (emptyGenesisBlock Time.epoch)

{------------------------------------------------------------------------------
  Useful combinators & helpers
------------------------------------------------------------------------------}

-- | Bracket-style initialiser for a SQLite DB.
withSqliteDB
    :: ( IsCrypto c
       , Show a
       )
    => (Block c (RadTx c) (Sealed c DummySeal) -> Gen a)
    -> (a -> Sqlite.Handle c (RadTx c) DummySeal -> IO ())
    -> Property
withSqliteDB genTestData action = monadicIO $ do
    testData <- pick (genTestData defaultGenesis)
    liftIO $
        bracket (open ":memory:" blockScore >>= initialize defaultGenesis)
                close
                (action testData)

-- | Bracket-style initialiser for a SQLite-backed 'BlockStore'.
withMemStore
    :: ( IsCrypto c
       , Show a
       )
    => (Block c (RadTx c) (Sealed c DummySeal) -> Gen a)
    -> (a -> Abstract.BlockStore c (RadTx c) DummySeal IO -> IO ())
    -> Property
withMemStore genTestData action = monadicIO $ do
    testData <- pick (genTestData defaultGenesis)
    liftIO $
        Sqlite.withBlockStore ":memory:" defaultGenesis blockScore $ action testData

-- | Inserts all the given blocks in the 'Orphanage'.
insertOrphans
    :: IsCrypto c
    => [Block c tx (Sealed c s)]
    -> Orphanage c tx s
    -> Orphanage c tx s
insertOrphans xs o =
    foldl' (flip insertOrphan) o xs

-- | Iterate over all the input chains and select the one with the best score.
bestChainOracle
    :: IsCrypto c
    => BlockHash c
    -- ^ The hash of the adopted block this chain originates from.
    -> [(Shuffled (Blockchain c tx s), Blockchain c tx s, Block c tx (Sealed c s))]
    -> Maybe (ChainCandidate c s)
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
shuffledOrphanChains
    :: ( IsCrypto c
       , Arbitrary tx
       , Arbitrary s
       , Serialise tx
       , Serialise s
       )
    => ForkParams
    -> Blockchain c tx s
    -> Gen [(Shuffled (Blockchain c tx s), Blockchain c tx s, Block c tx (Sealed c s))]
shuffledOrphanChains params initialChain = do
    xs <- genOrphanChainsFrom params initialChain
    forM xs (\(c,b) -> (,c,b) . Shuffled . unsafeToBlockchain <$> shuffle (blocks c))
