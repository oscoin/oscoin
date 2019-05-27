{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a toy implementation of a ledger. It consist
-- of a 'DummyState', 'DummyTx', and 'DummyOutput' together with
-- 'dummyEval'.
--
-- Transactions are 8 bytes of random data and the world state is the
-- list of all transactions starting with the most recent one. The
-- output of a transaction is just the transaction itself.
module Test.Oscoin.DummyLedger
    ( DummyTx(..)
    , DummyState
    , DummyOutput
    , dummyEvalBlock
    ) where


import           Oscoin.Prelude
import           Prelude (Show(..))

import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (HasHashing, Hashable(..))
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Block.SQLite

import           Codec.Serialise (Serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.BaseN as BaseN

import           Test.QuickCheck
import           Test.QuickCheck.Gen (chooseAny)

newtype DummyTx = DummyTx ByteString
    deriving (Eq, Ord, Serialise)

-- Displays the base58-encoded 8 byte payload
instance Show DummyTx where
    show (DummyTx dat) = "DummyTx " <> toS (BaseN.encodedText (BaseN.encodeBase58btc dat))

instance Arbitrary DummyTx where
    -- This needs to guarantee that with high probability all generated
    -- transactions have distinct hashes.
    arbitrary = DummyTx . BS.pack <$> vectorOf 8 (chooseAny :: Gen Word8)

instance (HasHashing c) => Hashable c DummyTx where
    hash (DummyTx payload) = Crypto.toHashed $ Crypto.hashByteArray payload

instance IsTxRow DummyTx where
    toTxRow (DummyTx msg) = TxRow
        { txRowContext = mempty
        , txRowMessage = msg
        , txRowAuthor  = mempty
        , txRowChainId = 0
        , txRowNonce   = 0
        }

    fromTxRow TxRow{txRowMessage} = Right $ DummyTx txRowMessage

-- | 'DummyState' is the list of all transactions with the most recent
-- transaction first.
type DummyState = [DummyTx]

type DummyOutput = DummyTx

-- | Prepends the transaction to the state and output the transaction.
dummyEvalBlock :: Evaluator c DummyState DummyTx DummyOutput
dummyEvalBlock _beneficiary txs st = (map Right txs, reverse txs <> st )
