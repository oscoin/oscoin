module Oscoin.Tests
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import           Oscoin.Crypto.Blockchain
                 (Blockchain(..), genesis, height, tip, unsafeToBlockchain)
import           Oscoin.Crypto.Blockchain.Block (Block(..), blockHash)
import           Oscoin.Crypto.Blockchain.Eval (evalBlockchain)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Event as Mempool
import           Oscoin.Storage.Block (BlockStore(..))
import qualified Oscoin.Storage.Block as BlockStore

import qualified Oscoin.Test.API as API
import qualified Oscoin.Test.API.HTTP as HTTP
import qualified Oscoin.Test.CLI as CLI
import qualified Oscoin.Test.Consensus as Consensus
import           Oscoin.Test.Crypto.Blockchain (testBlockchain)
import           Oscoin.Test.Crypto.Blockchain.Arbitrary
                 (arbitraryValidBlockchain)
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitrarySigned)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import qualified Oscoin.Test.P2P as P2P
import qualified Oscoin.Test.Storage.Block as BlockStore
import           Oscoin.Test.Storage.Block.Arbitrary ()

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck

import qualified Data.Aeson as Aeson
import           Data.Default (def)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

tests :: TestTree
tests = testGroup "Oscoin"
    [ testGroup      "API.HTTP"                       HTTP.tests
    -- ^ Testing HTTP API constructing HTTP requests manually
    , testGroup      "API"                            API.tests
    -- ^ Testing API and Node using a 'MonadClient' instance
    , testGroup      "CLI"                            CLI.tests
    , testCase       "Crypto"                         testOscoinCrypto
    , testCase       "Mempool"                        testOscoinMempool
    , testCase       "BlockStore lookup block"        testBlockStoreLookupBlock
    , testProperty   "BlockStore"                     (propOscoinBlockStore arbitraryValidBlockchain)
    , testProperty   "JSON instance of Hashed"        propHashedJSON
    , testProperty   "JSON instance of Signed"        propSignedJSON
    , testGroup      "Consensus"                      Consensus.tests
    , testGroup      "P2P"                            P2P.tests
    , testGroup      "Storage"                        BlockStore.tests
    , testBlockchain
    ]

testOscoinCrypto :: Assertion
testOscoinCrypto = do
    let val :: Text = "fnord"
    (_, priKey) <- Crypto.generateKeyPair
    signed <- Crypto.sign priKey val

    -- Verify that hashing a signed message is the same thing as hashing an
    -- unsigned one.
    Crypto.fromHashed (Crypto.hash signed) @?=
        Crypto.fromHashed (Crypto.hash val)

testOscoinMempool :: Assertion
testOscoinMempool = do
    -- Create a new mempool of account transactions.
    mp <- Mempool.newIO

    -- Create some arbitrary transactions.
    txs <- generate arbitrary :: IO [API.RadTx]

    -- Subscribe to the mempool with the subscription tokens.
    chan1 <- atomically $ Mempool.subscribe mp
    chan2 <- atomically $ Mempool.subscribe mp

    -- Add the transactions.
    atomically $ Mempool.insertMany mp txs

    -- Retrieve all events from the channels.
    evs1 <- atomically $ Mempool.drainChannel chan1
    evs2 <- atomically $ Mempool.drainChannel chan2

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

testBlockStoreLookupBlock :: Assertion
testBlockStoreLookupBlock = do
    blks <- generate $ arbitraryValidBlockchain @() @()
    let g  = genesis blks
    let bs = BlockStore { bsChains = Map.singleton (blockHash (tip blks)) blks
                        , bsOrphans = mempty
                        , bsScoreFn = comparing height }
    BlockStore.lookupBlock (blockHash g) bs @?= Just g

propOscoinBlockStore
    :: Gen (Blockchain (Seq Word8) (Seq Word8))
    -> Property
propOscoinBlockStore chainGen =
    forAll chainGen $ \chain -> do
        let foldEval x xs = Right ((), xs <> x)
        let blks = NonEmpty.toList $ fromBlockchain chain
        let bs   = BlockStore.fromOrphans blks (genesis chain)
        let best = BlockStore.getBlocks (fromIntegral $ height chain) bs
        let z    = snd $ evalBlockchain foldEval def (unsafeToBlockchain best)
        let txs  = concatMap (toList . blockData) $ reverse blks
        let txs' = mconcat txs
        counterexample ("From input: " ++ show txs ++ ", Expected: "
                                       ++ show txs' ++ " but got "
                                       ++ show z ++ ", " ++ show best)
                       (txs' == z)

propHashedJSON :: ByteString -> Bool
propHashedJSON bs =
    let x = Crypto.hash bs
     in (Aeson.decode . Aeson.encode) x == Just x

propSignedJSON :: Property
propSignedJSON =
    forAll (arbitrarySigned :: Gen (Crypto.Signed Text)) $ \msg ->
        (Aeson.decode . Aeson.encode) msg == Just msg
