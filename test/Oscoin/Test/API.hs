module Oscoin.Test.API where

import           Oscoin.Prelude hiding (get, state)

import           Oscoin.API.HTTP.Internal (MediaType(..), fromMediaType)
import qualified Oscoin.API.Types as API
import           Oscoin.Crypto.Blockchain
                 (BlockHash, Blockchain(..), blocks, genesis)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Hash (Hashed)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx (txPubKey)
import           Oscoin.Test.Crypto.Blockchain.Arbitrary
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.HTTP.Helpers

import           Data.Default (def)
import qualified Data.Text as T

import           Network.HTTP.Media ((//))
import           Network.HTTP.Types.Status
import qualified Network.Wai.Test as Wai
import           Numeric.Natural
import           Web.HttpApiData (toUrlPiece)

import qualified Radicle.Extended as Rad hiding (Env)

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
        [ test "No depth specified" getBestChain
        ]
    , testGroup "GET /state"
        [ test "Missing key" getMissingStateKey
        , test "Existing key" getExistingStateKey
        , test "Derefernce key" getReference
        ]
    ]
  where
    test name mkTest = testGroup name $ do
        let ctypes  = fromMediaType <$> [ JSON, CBOR ]
        let accepts = ("*" // "*") : ctypes
        codec <- [ newCodec accept content | content <- ctypes, accept <- accepts ]
        [testCase (T.unpack $ prettyCodec codec) $ mkTest codec >>=
            \HTTPTest{..} -> withNode testState $ runSession testSession]

data HTTPTest = HTTPTest
    { testState   :: NodeState
    , testSession :: Session ()
    }

httpTest :: NodeState -> Session () -> IO HTTPTest
httpTest state sess = pure $ HTTPTest{ testState = state, testSession = sess }

postTransactionWithInvalidSignature :: Codec -> IO HTTPTest
postTransactionWithInvalidSignature codec = httpTest emptyNodeState $ do
    (_, tx) <- liftIO $ genDummyTx
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

    (txHash, tx) <- liftIO $ genDummyTx
    -- Submit the transaction to the mempool.
    post codec "/transactions" tx >>=
        assertStatus accepted202 <>
        assertResultOK (API.TxSubmitResponse $ Crypto.hash tx)

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
    let missing = toUrlPiece (Crypto.zeroHash :: Crypto.Hash)
    get codec ("/transactions/" <> missing) >>= assertStatus notFound404

getConfirmedTransaction :: Codec -> IO HTTPTest
getConfirmedTransaction codec = do
    chain <- generate $ arbitraryValidBlockchain def
    case oldestTx chain of
        Nothing -> panic "No oldestTx found in chain"
        Just (tx, blkHash, confirmations) -> do
            let txHash = Crypto.hash tx

            httpTest (nodeState mempty chain) $
                getTransactionReturns codec txHash $ API.TxLookupResponse
                    { txHash = Crypto.hash tx
                    , txBlockHash = Just blkHash
                    , txConfirmations = confirmations
                    , txPayload = tx
                    }

oldestTx :: Blockchain tx s -> Maybe (tx, BlockHash, Natural)
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

getTransactionReturns :: Codec -> Hashed API.RadTx -> API.TxLookupResponse -> Wai.Session ()
getTransactionReturns codec txHash expected =
    get codec ("/transactions/" <> toUrlPiece (Crypto.fromHashed txHash)) >>=
    assertStatus ok200 <>
    assertResultOK expected

getMissingBlock :: Codec -> IO HTTPTest
getMissingBlock codec = httpTest emptyNodeState $ do
    get codec "/blocks/not-a-hash" >>= assertStatus notFound404

    let missing = toUrlPiece (Crypto.zeroHash :: Crypto.Hash)
    get codec ("/blocks/" <> missing) >>= assertStatus notFound404

getExistingBlock :: Codec -> IO HTTPTest
getExistingBlock codec = do
    chain <- generate $ arbitraryValidBlockchain def
    let g = void $ genesis chain
    let blockId = toUrlPiece . Crypto.fromHashed $ blockHash g

    httpTest (nodeState mempty chain) $
        get codec ("/blocks/" <> blockId) >>=
            assertStatus ok200 <>
            assertResultOK g

getMissingStateKey :: Codec -> IO HTTPTest
getMissingStateKey codec = httpTest emptyNodeState $
    get codec "/state?q=[not,found]" >>= assertStatus notFound404

getBestChain :: Codec -> IO HTTPTest
getBestChain codec = do
    chain <- generate $ arbitraryValidBlockchain def

    httpTest (nodeState mempty chain) $ do
        get codec "/blockchain/best?depth=1" >>=
            assertStatus ok200 <>
            assertResultOK (map void $ take 1 $ blocks chain)

        get codec "/blockchain/best" >>=
            assertStatus ok200 <>
            assertResultOK (map void $ take 3 $ blocks chain)

getExistingStateKey :: Codec -> IO HTTPTest
getExistingStateKey codec = do
    let env = initRadicleEnv [("my/key/path", Rad.String "hooray!")]

    httpTest (nodeState mempty $ blockchainFromEnv env) $
        get codec "/state?q=[my,key,path]" >>=
            assertStatus ok200 <>
            assertResultOK (Rad.toRad ("hooray!" :: Text))

getReference :: Codec -> IO HTTPTest
getReference codec = do
    let env = initRadicleEnv []
              & addRadicleRef "my-ref" (Rad.String "hooray!")
    httpTest (nodeState mempty $ blockchainFromEnv env) $
        get codec "/state?q=[my-ref]" >>=
            assertStatus ok200 <>
            assertResultOK (Rad.toRad ("hooray!" :: Text))

notTested :: IO HTTPTest
notTested = httpTest emptyNodeState $ liftIO $ assertFailure "Not tested"
