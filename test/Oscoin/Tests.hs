module Oscoin.Tests
    ( tests
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import qualified Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Nakamoto (blockScore)
import           Oscoin.Crypto.Blockchain
                 (Blockchain(..), genesis, height, unsafeToBlockchain)
import           Oscoin.Crypto.Blockchain.Block (Block(..), blockHash)
import           Oscoin.Crypto.Blockchain.Eval (evalBlockchain)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Node.Mempool as Mempool
import qualified Oscoin.Node.Mempool.Event as Mempool
import qualified Oscoin.Storage.Block.Pure as BlockStore

import qualified Oscoin.Test.API as API
import qualified Oscoin.Test.API.HTTP as HTTP
import qualified Oscoin.Test.CLI as CLI
import qualified Oscoin.Test.Consensus as Consensus
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain (testBlockchain)
import           Oscoin.Test.Crypto.Blockchain.Arbitrary (arbitraryBlockchain)
import           Oscoin.Test.Crypto.PubKey.Arbitrary
                 (arbitraryKeyPair, arbitrarySigned)
import           Oscoin.Test.Data.Rad.Arbitrary ()
import           Oscoin.Test.Data.Tx.Arbitrary ()
import qualified Oscoin.Test.Environment as Environment
import qualified Oscoin.Test.P2P as P2P
import qualified Oscoin.Test.Storage.Block.Equivalence as StorageEquivalence
import qualified Oscoin.Test.Storage.Block.Orphanage as Orphanage
import qualified Oscoin.Test.Storage.Block.SQLite.Blackbox as SQLite.Blackbox
import qualified Oscoin.Test.Storage.Block.SQLite.Whitebox as SQLite.Whitebox
import qualified Oscoin.Test.Telemetry as Telemetry
import           Oscoin.Test.Util (condensed)

import           Test.QuickCheck.Extended
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck hiding ((===))

import qualified Codec.Serialise as CBOR
import qualified Data.Aeson as Aeson
import           Data.ByteArray.Orphans ()
import qualified Data.ByteString as BS
import           Data.Default (def)
import qualified Data.List.NonEmpty as NonEmpty

tests :: forall c. Dict (IsCrypto c) -> Consensus.Config -> TestTree
tests d@Dict config = testGroup "Oscoin"
    [ testGroup      "API.HTTP"                       (HTTP.tests d)
    -- ^ Testing HTTP API constructing HTTP requests manually
    , testGroup      "API"                            (API.tests d)
    -- ^ Testing API and Node using a 'MonadClient' instance
    , testGroup      "CLI"                            CLI.tests
    , testGroup      "Crypto"                         (testOscoinCrypto d)
    , testProperty   "Mempool"                        (testOscoinMempool d)
    , testProperty   "BlockStore lookup block"        (testBlockStoreLookupBlock d)
    , testProperty   "BlockStore"                     (propOscoinBlockStore (arbitraryBlockchain @c))
    , testProperty   "JSON instance of Hashed"        (propHashedJSON d)
    , testProperty   "JSON instance of Signed"        (propSignedJSON d)
    , testGroup      "Consensus"                      (Consensus.tests d config)
    , testGroup      "P2P"                            (P2P.tests d)
    , testGroup      "Storage Orphanage"              (Orphanage.tests d)
    , testGroup      "Storage equivalence checking"   (StorageEquivalence.tests d)
    , testGroup      "SQLite Storage blackbox"        (SQLite.Blackbox.tests d)
    , testGroup      "SQLite Storage whitebox"        (SQLite.Whitebox.tests d)
    , testBlockchain d config
    , testGroup      "Telemetry"                      (Telemetry.tests d)
    , testGroup      "Environment"                    Environment.tests
    ]

testOscoinCrypto :: Dict (IsCrypto c) -> [TestTree]
testOscoinCrypto d = [
      testCase     "fnord roundtrip" (testSignRoundtrip d)
    , testProperty "CBOR.decode Hash . CBOR.encode Hash  == id" (testSerialiseHashRoundtrip d)
    , testProperty "CBOR.decode Signed . CBOR.encode Signed  == id" (testSerialiseSignedRoundtrip d)
    , testProperty "CBOR.decode PK . CBOR.encode PK  == id" (testSerialisePkRoundtrip d)
    , testProperty "JSON.fromJSON  PK . JSON.toJSON PK  == id" (testJsonPkRoundtrip d)
    ]

testSerialiseHashRoundtrip :: forall c. Dict (IsCrypto c) -> Property
testSerialiseHashRoundtrip Dict =
    forAll (arbitrary @ByteString) $ \bytes ->
        (BS.length bytes > 0) ==>
            let h = Crypto.hash @c bytes
            in (CBOR.deserialise . CBOR.serialise $ h) === h

testSerialiseSignedRoundtrip :: forall c. Dict (IsCrypto c) -> Property
testSerialiseSignedRoundtrip Dict =
    forAll (arbitrarySigned (Proxy @c) :: Gen (Crypto.Signed c Text)) $ \msg ->
        (CBOR.deserialise . CBOR.serialise $ msg) === msg

testSerialisePkRoundtrip :: forall c. Dict (IsCrypto c) -> Property
testSerialisePkRoundtrip Dict =
    forAllShow (arbitraryKeyPair @c) (show . bimap identity condensed) $ \(pk, _) ->
        (CBOR.deserialise . CBOR.serialise $ pk) === pk

testJsonPkRoundtrip :: forall c. Dict (IsCrypto c) -> Property
testJsonPkRoundtrip Dict =
    forAllShow (arbitraryKeyPair @c) (show . bimap identity condensed) $ \(pk, _) ->
        (Aeson.eitherDecode . Aeson.encode $ pk) === Right pk

testSignRoundtrip :: forall c. Dict (IsCrypto c) -> Assertion
testSignRoundtrip Dict = do
    let val :: Text = "fnord"
    (_, priKey) <- Crypto.generateKeyPair @c
    signed <- Crypto.sign priKey val

    -- Verify that hashing a signed message is the same thing as hashing an
    -- unsigned one.
    Crypto.fromHashed (Crypto.hash @c signed) @?=
        Crypto.fromHashed (Crypto.hash val)

testOscoinMempool :: forall c. Dict (IsCrypto c) -> Property
testOscoinMempool Dict = monadicIO $ do
    -- Create a new mempool of account transactions.
    mp <- lift (Mempool.newIO @c)

    -- Create some arbitrary transactions.
    txs <- pick (arbitrary @[API.RadTx c])

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

testBlockStoreLookupBlock :: forall c. Dict (IsCrypto c) -> Property
testBlockStoreLookupBlock Dict = monadicIO $ do
    blks <- pick (arbitraryBlockchain @c @() @())
    let g  = genesis blks
    let bs = BlockStore.initWithChain blks blockScore
    liftIO $ BlockStore.lookupBlock (blockHash g) bs @?= Just g

propOscoinBlockStore
    :: forall c. IsCrypto c
    => Gen (Blockchain c (Seq Word8) (Seq Word8))
    -> Property
propOscoinBlockStore chainGen =
    forAll (resize 10 chainGen) $ \chain -> do
        let foldEval x xs = Right ((), xs <> x)
        let blks = NonEmpty.toList $ fromBlockchain chain
        let bs   = BlockStore.fromOrphans blks (genesis chain) blockScore
        let best = BlockStore.getBlocks (fromIntegral $ height chain) bs
        let z    = snd $ evalBlockchain foldEval def (unsafeToBlockchain best)
        let txs  = concatMap (toList . blockData) $ reverse blks
        let txs' = mconcat txs
        counterexample ("From input: " ++ show txs ++ ", Expected: "
                                       ++ show txs' ++ " but got "
                                       ++ show z ++ ", " ++ show best)
                       (txs' == z)

propHashedJSON :: forall c. Dict (IsCrypto c) -> ByteString -> Bool
propHashedJSON Dict bs =
    let x = Crypto.hash @c bs
     in (Aeson.decode . Aeson.encode) x == Just x

propSignedJSON :: forall c. Dict (IsCrypto c) -> Property
propSignedJSON Dict =
    forAll (arbitrarySigned (Proxy @c) :: Gen (Crypto.Signed c Text)) $ \msg ->
          (Aeson.eitherDecode . Aeson.encode) msg === Right msg
