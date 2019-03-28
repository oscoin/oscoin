{-# LANGUAGE UndecidableInstances #-}

module Test.Oscoin.DummyLedger
    ( DummyTx(..)
    , DummyState
    , DummyOutput
    , dummyEval
    ) where


import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Eval (Evaluator)
import           Oscoin.Crypto.Hash (HasHashing, Hashable(..))
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Storage.Block.SQLite

import           Codec.Serialise (Serialise)
import qualified Data.ByteString as BS

import           Test.QuickCheck

newtype DummyTx = DummyTx ByteString
    deriving (Eq, Ord, Show, Serialise)

instance Arbitrary DummyTx where
    arbitrary = DummyTx . BS.pack <$> vectorOf 32 arbitrary

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

type DummyState = ()

type DummyOutput = ()

dummyEval :: Evaluator DummyState DummyTx DummyOutput
dummyEval _ s = Right ((), s)
