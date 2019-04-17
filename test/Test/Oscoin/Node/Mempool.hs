module Test.Oscoin.Node.Mempool
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Data.Tx
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Event as Mempool

import           Oscoin.Test.Crypto
import           Oscoin.Test.Data.Tx.Arbitrary ()

import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Node.Mempool"
    [ testProperty "prop_mempool" (prop_mempool d) ]

prop_mempool :: forall c. Dict (IsCrypto c) -> Property
prop_mempool Dict = monadicIO $ do
    -- Create a new mempool of account transactions.
    mp <- lift (Mempool.newIO @c)

    -- Create some arbitrary transactions.
    txs <- pick (arbitrary @[Tx c])

    -- Subscribe to the mempool with the subscription tokens.
    (chan1, chan2, evs1, evs2) <- lift $ atomically $ do
        chan1 <- Mempool.subscribe mp
        chan2 <- Mempool.subscribe mp

        -- Add the transactions.
        Mempool.insertMany mp txs

        -- Retrieve all events from the channels.
        evs1 <- Mempool.drainChannel chan1
        evs2 <- Mempool.drainChannel chan2

        pure (chan1, chan2, evs1, evs2)

    liftIO $ do
        -- Verify that they contain the transactions we generated.
        assertEqual "Chan 1 has all Txs" txs $ concat . mapMaybe fromEvent $ evs1
        assertEqual "Chan 2 has all Txs" txs $ concat . mapMaybe fromEvent $ evs2

        -- If we try to read again, the channel is empty.
        atomically (Mempool.drainChannel chan1) >>= assertEqual "Chan 1 is empty" []
        atomically (Mempool.drainChannel chan2) >>= assertEqual "Chan 2 is empty" []
  where
    fromEvent :: Mempool.Event tx -> Maybe [tx]
    fromEvent (Mempool.Insert txs) = Just txs
    fromEvent _                    = Nothing
