module Oscoin.Test.Storage.Block.SQLite
    ( defaultGenesis
    , DummySeal
    , withSqliteDB
    , withMemStore
    ) where

import           Oscoin.Prelude


import           Oscoin.Crypto.Blockchain hiding (genesisBlock)
import           Oscoin.Data.RadicleTx
import qualified Oscoin.Storage.Block.Abstract as Abstract
import           Oscoin.Storage.Block.SQLite as Sqlite
import           Oscoin.Storage.Block.SQLite.Internal as Sqlite
import qualified Oscoin.Time as Time

import           Oscoin.Test.Crypto
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
        bracket (open ":memory:" >>= initialize defaultGenesis)
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
        Sqlite.withBlockStore ":memory:" defaultGenesis $ action testData
