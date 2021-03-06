{- | Whitebox testing for the SQLite storage backend by using
     explicitly its internal functions
-}
module Oscoin.Test.Storage.Block.SQLite.Whitebox
    ( tests
    ) where

import           Oscoin.Prelude

import           Data.ByteArray.Orphans ()

import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Data.OscoinTx
import           Oscoin.Storage.Block.SQLite.Internal as Sqlite

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary ()
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.Storage.Block.SQLite

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

tests
    :: forall c.
    ( Show (TxPayload c)
    , Arbitrary (Tx c)
    ) => Dict (IsCrypto c) -> [TestTree]
tests Dict =
    [ testGroup "Storage.Block.SQLite whitebox testing (internals)"
        [ testProperty "isStored"           (withSqliteDB @c (const arbitrary) testIsStored)
        , testProperty "isConflicting"      (withSqliteDB @c (\g -> (,) <$> genBlockFrom @c g
                                                                        <*> genBlockFrom @c g
                                                          ) testIsConflicting)
        ]
    ]

{------------------------------------------------------------------------------
  The tests proper
------------------------------------------------------------------------------}

testIsStored
    :: forall c. (IsCrypto c)
    => ()
    -> Sqlite.Handle c (Tx c) DummySeal
    -> Assertion
testIsStored () h = do
    (gen :: Block c (Tx c) (Sealed c DummySeal)) <- getGenesisBlock h

    result <- runTransaction (hConn h) $ isStored (blockHash gen)
    result @?= True

    result' <- runTransaction (hConn h) $ isStored (Crypto.zeroHash @c)
    result' @?= False

testIsConflicting
    :: (IsCrypto c)
    => ( Block c (Tx c) (Sealed c DummySeal)
       , Block c (Tx c) (Sealed c DummySeal)
       )
    -> Sqlite.Handle c (Tx c) DummySeal
    -> Assertion
testIsConflicting (blk, blk') h@Handle{..} = do
    storeBlock h blk

    result <- runTransaction hConn $ isConflicting blk'
    result @?= True

    -- Linking parents doesn't affect the height, so the block is still
    -- considered as conflicting with a previously-stored one.
    result' <- runTransaction hConn $ isConflicting (linkParent blk blk')
    result' @?= True
