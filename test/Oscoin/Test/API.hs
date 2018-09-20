module Oscoin.Test.API where

import           Oscoin.Prelude

import           Oscoin.API.HTTP.Internal (MediaType(..), fromMediaType)
import qualified Oscoin.API.Types as API
import           Oscoin.Consensus.Evaluator (constEval)
import           Oscoin.Crypto.Blockchain
                 (BlockHash, Blockchain(..), blocks, genesis, takeBlocks)
import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txPubKey)
import qualified Oscoin.Node as Node
import           Oscoin.Test.Crypto.Blockchain.Arbitrary
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.HTTP.Helpers

import qualified Data.Text as T

import           Network.HTTP.Media ((//))
import           Network.HTTP.Types.Status
import qualified Network.Wai.Test as Wai

import qualified Radicle as Rad

import           Test.QuickCheck (generate)
import           Test.Tasty
import           Test.Tasty.HUnit (assertFailure, testCase)

tests :: [TestTree]
tests =
    [ test "Smoke test" smokeTestOscoinAPI
    , testGroup "GET /transactions/:hash"
        [ testGroup "404 Not Found"
            [ test "Missing transaction" getMissingTransaction ]

        , testGroup "200 OK"
            [ test "Unconfirmed transaction" getUnconfirmedTransaction
            , test "Confirmed transaction" getConfirmedTransaction
            ]
        ]
    , testGroup "POST /transactions"
        [ testGroup "400 Bad Request"
            [ test "Invalid signature" postTransactionWithInvalidSignature]
        ]
    , testGroup "GET /blocks/:hash"
        [ test "Missing block" getMissingBlock
        , test "Existing block" getExistingBlock
        ]
    , testGroup "GET /blockchain/best"
        [ test "No depth specified" getBestChain ]
    , testGroup "GET /state"
        [ test "Missing key" getMissingStateKey
        , test "Existing key" getExistingStateKey
        ]
    ]
  where
    test name mkTest = testGroup name $ do
        let ctypes  = fromMediaType <$> [ JSON, CBOR ]
        let accepts = ("*" // "*") : ctypes
        codec <- [ newCodec accept content | content <- ctypes, accept <- accepts ]
        [testCase (T.unpack $ prettyCodec codec) $ mkTest codec >>=
            \HTTPTest{..} -> makeNode testState >>= runSession testSession]

data HTTPTest = HTTPTest
    { testState   :: NodeState
    , testSession :: Session ()
    }

httpTest :: NodeState -> Session () -> IO HTTPTest
httpTest state sess = pure $ HTTPTest{ testState = state, testSession = sess }

postTransactionWithInvalidSignature :: Codec -> IO HTTPTest
postTransactionWithInvalidSignature codec = httpTest emptyNodeState $ do
    (_, tx) <- io $ genDummyTx
    otherPubKey <- fst <$> Crypto.generateKeyPair
    let tx' = tx { txPubKey = otherPubKey }
    post codec "/transactions" tx' >>=
        assertResultErr "Invalid transaction signature" <>
        assertStatus badRequest400

smokeTestOscoinAPI :: Codec -> IO HTTPTest
smokeTestOscoinAPI codec = httpTest emptyNodeState $ do
    get codec "/" >>= assertStatus ok200

    -- The mempool is empty.
    get codec "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK ([] @API.RadTx)

    (txHash, tx) <- io $ genDummyTx
    -- Submit the transaction to the mempool.
    post codec "/transactions" tx >>=
        assertStatus accepted202 <>
        assertResultOK Node.Receipt {fromReceipt = Crypto.hash tx}

    -- Get the mempool once again, make sure the transaction is in there.
    get codec "/transactions" >>=
        assertStatus ok200 <>
        assertResultOK [tx]

    getTransactionReturns codec txHash $ unconfirmedTx tx

getMissingTransaction :: Codec -> IO HTTPTest
getMissingTransaction codec = httpTest emptyNodeState $ do
    -- Malformed transaction hash returns a 404
    get codec "/transactions/not-a-hash" >>= assertStatus notFound404

    -- Well formed but missing transaction hash returns a 404
    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get codec ("/transactions/" <> missing) >>= assertStatus notFound404

getConfirmedTransaction :: Codec -> IO HTTPTest
getConfirmedTransaction codec = do
    chain <- generate $ arbitraryValidBlockchain
    let (tx, bh, confirmations) = oldestTx chain
    let txHash = decodeUtf8 $ Crypto.toHex $ Crypto.hash tx

    httpTest (nodeState mempty chain) $
        getTransactionReturns codec txHash $ API.TxLookupResponse
            { txHash = Crypto.hash tx
            , txBlockHash = Just bh
            , txConfirmations = confirmations
            , txPayload = tx
            }

oldestTx :: Blockchain tx s -> (tx, BlockHash, Word64)
oldestTx (blocks -> blks) = head $ do
    (i, blk) <- zip [1..] blks
    tx <- toList $ blockData blk
    pure (tx, headerHash $ blockHeader blk, i)

getUnconfirmedTransaction :: Codec -> IO HTTPTest
getUnconfirmedTransaction codec = do
    (txHash, tx) <- genDummyTx
    httpTest (nodeState [tx] emptyBlockstore) $
        getTransactionReturns codec txHash $ unconfirmedTx tx

unconfirmedTx :: API.RadTx -> API.TxLookupResponse
unconfirmedTx tx = API.TxLookupResponse
    { txHash = Crypto.hash tx
    , txBlockHash = Nothing
    , txConfirmations = 0
    , txPayload = tx
    }

getTransactionReturns :: Codec -> Text -> API.TxLookupResponse -> Wai.Session ()
getTransactionReturns codec txHash expected =
    get codec ("/transactions/" <> txHash) >>=
    assertStatus ok200 <>
    assertResultOK expected

getMissingBlock :: Codec -> IO HTTPTest
getMissingBlock codec = httpTest emptyNodeState $ do
    get codec "/blocks/not-a-hash" >>= assertStatus notFound404

    let missing = decodeUtf8 $ Crypto.toHex $ (Crypto.zeroHash :: Crypto.Hash)
    get codec ("/blocks/" <> missing) >>= assertStatus notFound404

getExistingBlock :: Codec -> IO HTTPTest
getExistingBlock codec = do
    chain <- generate $ arbitraryValidBlockchain
    let g = void $ genesis chain

    httpTest (nodeState mempty chain) $
        get codec ("/blocks/" <> decodeUtf8 (Crypto.toHex (blockHash g))) >>=
            assertStatus ok200 <>
            assertResultOK g

getBestChain :: Codec -> IO HTTPTest
getBestChain codec = do
    chain <- generate $ arbitraryValidBlockchain

    httpTest (nodeState mempty chain) $ do
        get codec "/blockchain/best?depth=1" >>=
            assertStatus ok200 <>
            assertResultOK (map void $ takeBlocks 1 chain)

        get codec "/blockchain/best" >>=
            assertStatus ok200 <>
            assertResultOK (map void $ takeBlocks 3 chain)

getMissingStateKey :: Codec -> IO HTTPTest
getMissingStateKey codec = httpTest emptyNodeState $
    get codec "/state?q=[not,found]" >>= assertStatus notFound404

getExistingStateKey :: Codec -> IO HTTPTest
getExistingStateKey codec = do
    let st = dummyRadicleEnv [("my/key/path", Rad.String "hooray!")]

    (_, tx) <- genDummyTx
    gen     <- generate $ arbitraryGenesisWith (constEval st) [tx]

    httpTest (nodeState mempty (Blockchain (gen :| []))) $
        get codec "/state?q=[my,key,path]" >>=
            assertStatus ok200 <>
            assertResultOK (Rad.String "hooray!")


notTested :: IO HTTPTest
notTested = httpTest emptyNodeState $ io $ assertFailure "Not tested"
