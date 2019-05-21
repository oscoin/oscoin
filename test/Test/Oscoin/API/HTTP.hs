module Test.Oscoin.API.HTTP
    ( tests
    ) where

import           Oscoin.Prelude hiding (get, state)

import           Oscoin.Crypto.Blockchain (blocks, genesis, tip)
import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Test.Crypto.Blockchain.Generators
import           Oscoin.Test.Data.Tx.Arbitrary ()
import           Oscoin.Test.HTTP.Helpers
import           Oscoin.Time.Chrono (toNewestFirst)

import           Network.HTTP.Types.Status
import           Web.HttpApiData (toUrlPiece)

import           Oscoin.Test.Crypto
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: forall c. Dict (IsCrypto c) -> TestTree
tests Dict = testGroup "Test.Oscoin.API.HTTP"
    [ testGroup "GET /blocks/:hash"
        [ test "Missing block" (getMissingBlock @c)
        , test "Existing block" (getExistingBlock @c)
        ]
    , testGroup "GET /blockchain/best"
        [ test "No depth specified" (getBestChain @c)
        ]
    , testGroup "GET /blockchain/tip"
        [ test "returns the tip of the blockchain" (getTip @c)
        ]
    ]
  where
    test name mkTest =
        testProperty name $ monadicIO $ do
            HTTPTest{..} <- mkTest
            liftIO $ runSession testState testSession

data HTTPTest c = HTTPTest
    { testState   :: NodeState c
    , testSession :: Session c ()
    }

httpTest :: NodeState c -> Session c () -> IO (HTTPTest c)
httpTest state sess = pure $ HTTPTest{ testState = state, testSession = sess }

getMissingBlock :: forall c. IsCrypto c => PropertyM IO (HTTPTest c)
getMissingBlock = liftIO $ httpTest emptyNodeState $ do
    get "/blocks/not-a-hash" >>= assertStatus notFound404

    let missing = toUrlPiece (Crypto.zeroHash :: Crypto.Hash c)
    get ("/blocks/" <> missing) >>= assertStatus notFound404

getExistingBlock :: IsCrypto c => PropertyM IO (HTTPTest c)
getExistingBlock = do
    chain <- pick genBlockchain
    let g = genesis chain
    let blockId = toUrlPiece $ blockHash g

    liftIO $ httpTest (nodeState mempty chain mempty) $
        get ("/blocks/" <> blockId) >>=
            assertStatus ok200 <>
            assertResultOK g

getBestChain :: IsCrypto c => PropertyM IO (HTTPTest c)
getBestChain = do
    chain <- pick genBlockchain

    liftIO $ httpTest (nodeState mempty chain mempty) $ do
        get "/blockchain/best?depth=1" >>=
            assertStatus ok200 <>
            assertResultOK (take 1 . toNewestFirst $ blocks chain)

        get "/blockchain/best" >>=
            assertStatus ok200 <>
            assertResultOK (take 3 . toNewestFirst $ blocks chain)

getTip :: IsCrypto c => PropertyM IO (HTTPTest c)
getTip = do
    chain <- pick genBlockchain

    liftIO $ httpTest (nodeState mempty chain mempty) $
        get "/blockchain/tip" >>=
            assertStatus ok200 <>
            assertResultOK (tip chain)
