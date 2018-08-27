module Oscoin.Tests where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.BlockStore as BlockStore
import           Oscoin.Consensus.Evaluator (foldEval, identityEval)
import           Oscoin.Crypto.Blockchain (Blockchain(..), genesis, height, tip, validateBlockchain)
import           Oscoin.Crypto.Blockchain.Block (Block(..), BlockHeader(..), blockHeader, toOrphan, validateBlock)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Environment (Environment(Testing))
import qualified Oscoin.Logging as Log
import qualified Oscoin.Node as Node
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Event as Mempool

import qualified Oscoin.Test.Consensus as Consensus
import           Oscoin.Test.Crypto.Blockchain.Arbitrary (arbitraryGenesisWith, arbitraryValidBlockWith, arbitraryValidBlockchain)
import           Oscoin.Test.Crypto.BlockStore.Arbitrary ()
import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitrarySignedWith, arbitrarySigned)
import           Oscoin.Test.Helpers
import           Oscoin.Test.HTTP.Helpers
import qualified Oscoin.Test.P2P as P2P

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit hiding ((@?=))
import           Test.Tasty.QuickCheck

import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (emptyArray)
import qualified Data.Binary as Binary
import qualified Data.List.NonEmpty as NonEmpty
import           Lens.Micro ((^?!))
import           Lens.Micro.Aeson (key, _String)

nodeConfig :: Node.Config
nodeConfig = Node.Config
    { Node.cfgServiceName = "http"
    , Node.cfgPeers       = []
    , Node.cfgEnv         = Testing
    , Node.cfgLogger      = Log.noLogger
    }

tests :: TestTree
tests = testGroup "Oscoin"
    [ testCase       "API"                            testOscoinAPI
    , testCase       "Crypto"                         testOscoinCrypto
    , testCase       "Mempool"                        testOscoinMempool
    , testCase       "Blockchain"                     testOscoinBlockchain
    , testProperty   "BlockStore"                     (propOscoinBlockStore arbitraryValidBlockchain)
    , testProperty   "Binary instance of Hashed"      propHashedBinary
    , testProperty   "JSON instance of Hashed"        propHashedJSON
    , testProperty   "JSON instance of Signed"        propSignedJSON
    , testProperty   "Hexadecimal encoding"           propHexEncoding
    , testGroup      "Consensus"                      Consensus.tests
    , testGroup      "P2P"                            P2P.tests
    ]

testOscoinAPI :: Assertion
testOscoinAPI = runSession nodeConfig 42 $ do
    get "/" >>= assertOK

    -- The mempool is empty.
    get "/node/mempool" >>= assertBody emptyArray

    -- Now let's create a transaction.
    let tx :: DummyTx = ()

    -- Now generate a key pair and sign the transaction.
    -- TODO(cloudhead): This doesn't work anymore.
    (_, priKey) <- Crypto.generateKeyPair
    tx'         <- Crypto.sign priKey tx

    -- Submit the transaction to the mempool.
    resp <- post "/node/mempool" tx' ; assertStatus 202 resp

    -- The response is a transaction receipt, with the transaction
    -- id (hash).
    let txId = responseBody resp ^?! key "tx" . _String

    -- Get the mempool once again, make sure the transaction is in there.
    _mp <- responseBody <$> get "/node/mempool"

    -- TODO(cloudhead): This doesn't work anymore.
    -- mp ^? nth 0 . key "id" . _String @?= Just txId

    get ("/node/mempool/" <> txId) >>= assertOK <> assertJSON

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
    txs <- generate arbitrary :: IO [DummyTx]

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

testOscoinBlockchain :: Assertion
testOscoinBlockchain = do
    (_, key') <- Crypto.generateKeyPair

    txs <- generate . listOf $
        arbitrarySignedWith key' :: IO [Crypto.Signed DummyTx]

    gblock <- generate $ arbitraryGenesisWith identityEval txs
    assertNoError $ validateBlock gblock

    txs' <- generate . listOf $
        arbitrarySignedWith key' :: IO [Crypto.Signed DummyTx]

    block <- generate $ arbitraryValidBlockWith (blockHeader gblock) txs'

    assertNoError $ validateBlockchain $ Blockchain (block :| [gblock])

propOscoinBlockStore
    :: Gen (Blockchain (Seq Word8) (Seq Word8))
    -> Property
propOscoinBlockStore chainGen =
    forAll chainGen $ \chain -> do
        let blks = NonEmpty.toList $ fromBlockchain chain
        let os   = map (toOrphan foldEval) blks
        let bs   = BlockStore.fromOrphans os (genesis chain)
        let best = BlockStore.maximumChainBy (comparing height) bs
        let z    = blockState $ blockHeader $ tip best
        let txs  = concatMap (toList . blockData) $ reverse blks
        counterexample ("From input: " ++ show txs ++ ", Expected: "
                                       ++ show (concat txs) ++ " but got "
                                       ++ show z ++ show best) (concat txs == z)

propHashedBinary :: Crypto.Hashed ByteString -> Bool
propHashedBinary x = (Binary.decode . Binary.encode) x == x

propHashedJSON :: Crypto.Hashed ByteString -> Bool
propHashedJSON x = (Aeson.decode . Aeson.encode) x == Just x

propHexEncoding :: ByteString -> Bool
propHexEncoding x = (Crypto.fromHex . Crypto.toHex) x == Right x

propSignedJSON :: Property
propSignedJSON =
    forAll (arbitrarySigned :: Gen (Crypto.Signed Text)) $ \msg ->
        (Aeson.decode . Aeson.encode) msg == Just msg
