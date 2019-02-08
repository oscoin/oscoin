module Oscoin.Test.Storage.Block.SQLite
    ( genGenesisLinkedBlock
    , defaultGenesis
    , DummySeal
    , withSqliteDB
    , withMemStore
    , withDifficulty
    ) where

import           Oscoin.Prelude

import           Codec.Serialise

import           Oscoin.Crypto.Blockchain
                 ( Difficulty
                 , blockScore
                 , blockTargetDifficulty
                 , emptyGenesisBlock
                 , headerHash
                 , sealBlock
                 )
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), blockTimestamp, linkParent)
import           Oscoin.Data.RadicleTx
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.SQLite as Sqlite
import           Oscoin.Storage.Block.SQLite.Internal as Sqlite
import qualified Oscoin.Time as Time

import           Oscoin.Test.Crypto.Blockchain.Arbitrary (arbitraryBlock)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

type DummySeal = Text

defaultGenesis :: Block tx DummySeal
defaultGenesis =
    sealBlock mempty (emptyGenesisBlock Time.epoch)

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

withDifficulty :: Serialise s => Difficulty -> Block tx s -> Block tx s
withDifficulty d blk =
    blk { blockHeader = header, blockHash = headerHash header }
  where
    header = (blockHeader blk) { blockTargetDifficulty = d }
