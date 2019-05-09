module Test.Oscoin.P2P.Handshake (tests, props, genKeyPair, genKeyPairPair) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey as Crypto
import           Oscoin.P2P.Handshake as Handshake
import           Oscoin.P2P.Types
                 ( Addr
                 , NodeInfo
                 , fromNodeId
                 , mkAddr
                 , mkNodeId
                 , mkNodeInfo
                 , nodeHttpApiAddr
                 , nodeNodeId
                 )

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.PubKey.Arbitrary (arbitraryKeyPairs)
import           Oscoin.Test.Util (Condensed, condensed)
import           Test.Oscoin.P2P.Gen (genHost, genNetwork)
import           Test.Oscoin.P2P.Helpers (framedPair)

import qualified Crypto.Noise.Exception as Noise

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Gen.QuickCheck (quickcheck)
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.P2P.Handshake"
    [ testProperty "prop_simple"                (prop_simple d)
    , testProperty "prop_simpleRejectsSelf"     (prop_simpleRejectsSelf d)
    , testProperty "prop_simpleNetworkMismatch" (prop_simpleNetworkMismatch d)
    , testProperty "prop_simpleApiInfoExchange" (prop_simpleApiInfoExchange d)
    , testProperty "prop_secure"                (prop_secure d)
    , testProperty "prop_secureRejectsSelf"     (prop_secureRejectsSelf d)
    , testProperty "prop_secureNetworkMismatch" (prop_secureNetworkMismatch d)
    , testProperty "prop_secureApiInfoExchange" (prop_secureApiInfoExchange d)
    ]

-- | For GHCi use.
props :: Dict (IsCrypto c) -> IO Bool
props d = checkParallel $ Group "Test.Oscoin.P2P.Handshake"
    [ ("prop_simple"               , prop_simple                d)
    , ("prop_simpleRejectsSelf"    , prop_simpleRejectsSelf     d)
    , ("prop_simpleNetworkMismatch", prop_simpleNetworkMismatch d)
    , ("prop_simpleApiInfoExchange", prop_simpleApiInfoExchange d)
    , ("prop_secure"               , prop_secure                d)
    , ("prop_secureRejectsSelf"    , prop_secureRejectsSelf     d)
    , ("prop_secureNetworkMismatch", prop_secureNetworkMismatch d)
    , ("prop_secureApiInfoExchange", prop_secureApiInfoExchange d)
    ]

prop_simple :: forall c. Dict (IsCrypto c) -> Property
prop_simple Dict = withTests 1 . property $ do
    network                                    <- forAll genNetwork
    (keysAlice@(pkAlice,_), keysBob@(pkBob,_)) <- genKeyPairPair @c

    let nidAlice = mkNodeId pkAlice
    infoAlice    <- forAll genAddr

    let nidBob   = mkNodeId pkBob
    infoBob     <- forAll genAddr

    let hsAlice  = Handshake.simpleHandshake @ByteString keysAlice infoAlice network
    let hsBob    = Handshake.simpleHandshake @ByteString keysBob   infoBob   network

    hrs <- runHandshake (mkNodeInfo infoAlice nidAlice) hsAlice hsBob
    case hrs of
        (Left e, _) -> annotateShow e *> failure
        (_, Left e) -> annotateShow e *> failure
        (Right hrAlice, Right hrBob) -> do
            -- Alice and Bob know the other's key, respectively
            nidHash (Handshake.hrPeerInfo hrAlice) === nidHash (mkNodeInfo infoBob nidBob)
            nidHash (Handshake.hrPeerInfo hrBob  ) === nidHash (mkNodeInfo infoAlice nidAlice)

            -- Alice and Bob sign outgoing protocol messages, which the other
            -- side verifies.
            let msg = "are we signed yet?"
            tripping'
                msg
                (evalIO . Handshake.hrPreSend  hrAlice)
                (evalIO . Handshake.hrPostRecv hrBob)

            tripping'
                msg
                (evalIO . Handshake.hrPreSend  hrBob)
                (evalIO . Handshake.hrPostRecv hrAlice)

            assertInvalidSignature msg $
                Handshake.hrPreSend hrAlice >=> Handshake.hrPostRecv hrAlice
            assertInvalidSignature msg $
                Handshake.hrPreSend hrBob   >=> Handshake.hrPostRecv hrBob
  where
    assertInvalidSignature x f =
        (=== Left Handshake.InvalidSignature) =<< evalIO (try (f x))

prop_simpleRejectsSelf :: forall c. Dict (IsCrypto c) -> Property
prop_simpleRejectsSelf Dict = withTests 1 . property $ do
    network  <- forAll genNetwork
    (pk, sk) <- genKeyPair @c

    let myNid = mkNodeId pk
    myInfo <- forAll genAddr

    let hsAlice = Handshake.simpleHandshake @ByteString (pk, sk) myInfo network
    let hsBob   = Handshake.simpleHandshake @ByteString (pk, sk) myInfo network

    hrs <- runHandshake (mkNodeInfo myInfo myNid) hsAlice hsBob
    case hrs of
        (Left Handshake.DuplicateId, _) -> success
        (_, Left Handshake.DuplicateId) -> success
        _                               -> failure

prop_simpleNetworkMismatch :: forall c. Dict (IsCrypto c) -> Property
prop_simpleNetworkMismatch Dict = withTests 1 . property $ do
    (netAlice, netBob) <- forAll $ do
        a <- genNetwork
        b <- Gen.filter (/= a) genNetwork
        pure (a, b)

    (keysAlice@(pkAlice,_), keysBob) <- genKeyPairPair @c

    let nidAlice = mkNodeId pkAlice
    infoAlice <- forAll genAddr

    infoBob   <- forAll genAddr

    let hsAlice = Handshake.simpleHandshake @ByteString keysAlice infoAlice netAlice
    let hsBob   = Handshake.simpleHandshake @ByteString keysBob   infoBob netBob

    hrs <- runHandshake (mkNodeInfo infoAlice nidAlice) hsAlice hsBob
    case hrs of
        ( Left Handshake.NetworkMismatch,
          Left Handshake.NetworkMismatch ) -> success
        _                                  -> failure

prop_simpleApiInfoExchange :: forall c. Dict (IsCrypto c) -> Property
prop_simpleApiInfoExchange Dict = withTests 1 . property $ do
    network                          <- forAll genNetwork
    (keysAlice@(pkAlice,_), keysBob) <- genKeyPairPair @c

    let nidAlice = mkNodeId pkAlice
    infoAlice    <- forAll genAddr

    infoBob     <- forAll genAddr

    let hsAlice  = Handshake.simpleHandshake @ByteString keysAlice infoAlice network
    let hsBob    = Handshake.simpleHandshake @ByteString keysBob   infoBob   network

    hrs <- runHandshake (mkNodeInfo infoAlice nidAlice) hsAlice hsBob
    case hrs of
        (Left e, _) -> annotateShow e *> failure
        (_, Left e) -> annotateShow e *> failure
        (Right hrAlice, Right hrBob) -> do
            -- Alice and Bob know the other's http API info, respectively
            nodeHttpApiAddr (Handshake.hrPeerInfo hrAlice) === infoBob
            nodeHttpApiAddr (Handshake.hrPeerInfo hrBob)   === infoAlice

prop_secure :: forall c. Dict (IsCrypto c) -> Property
prop_secure Dict = withTests 1 . property $ do
    network                            <- forAll genNetwork
    ((pkAlice,skAlice), (pkBob,skBob)) <- genKeyPairPair @c

    infoAlice <- forAll $ genNodeInfo pkAlice
    infoBob   <- forAll $ genNodeInfo pkBob

    let hsAlice = Handshake.secureHandshake @LByteString skAlice infoAlice network
    let hsBob   = Handshake.secureHandshake @LByteString skBob   infoBob   network

    hrs <- runHandshake infoAlice hsAlice hsBob
    case hrs of
        (Left e, _) -> annotateShow e *> failure
        (_, Left e) -> annotateShow e *> failure
        (Right hrAlice, Right hrBob) -> do
            -- Alice and Bob know the other's key, respectively
            nidHash (Handshake.hrPeerInfo hrAlice) === nidHash infoBob
            nidHash (Handshake.hrPeerInfo hrBob  ) === nidHash infoAlice

            -- Payload messages are encrypted
            let msg = "are we encrypted yet?"
            tripping'
                msg
                (evalIO . Handshake.hrPreSend  hrAlice)
                (evalIO . Handshake.hrPostRecv hrBob)

            tripping'
                msg
                (evalIO . Handshake.hrPreSend  hrBob)
                (evalIO . Handshake.hrPostRecv hrAlice)

            assertDecryptionError msg $
                Handshake.hrPreSend hrAlice >=> Handshake.hrPostRecv hrAlice
            assertDecryptionError msg $
                Handshake.hrPreSend hrBob   >=> Handshake.hrPostRecv hrBob
  where
    assertDecryptionError x f = do
        res <- liftIO $ try (f x)
        case res of
            Left Noise.DecryptionError -> success
            _                          -> failure

prop_secureRejectsSelf :: forall c. Dict (IsCrypto c) -> Property
prop_secureRejectsSelf Dict = withTests 1 . property $ do
    network  <- forAll genNetwork
    (pk, sk) <- genKeyPair @c
    myInfo   <- forAll $ genNodeInfo pk

    let hsAlice = Handshake.secureHandshake @ByteString sk myInfo network
    let hsBob   = Handshake.secureHandshake @ByteString sk myInfo network

    hrs <- runHandshake myInfo hsAlice hsBob
    case hrs of
        (Left (Handshake.SimpleHandshakeError Handshake.DuplicateId), _) -> success
        (_, Left (Handshake.SimpleHandshakeError Handshake.DuplicateId)) -> success
        _                                                                -> failure

prop_secureNetworkMismatch :: forall c. Dict (IsCrypto c) -> Property
prop_secureNetworkMismatch Dict = withTests 1 . property $ do
    (netAlice, netBob) <- forAll $ do
        a <- genNetwork
        b <- Gen.filter (/= a) genNetwork
        pure (a, b)

    ((pkAlice,skAlice), (pkBob,skBob)) <- genKeyPairPair @c

    infoAlice <- forAll $ genNodeInfo pkAlice
    infoBob   <- forAll $ genNodeInfo pkBob

    let hsAlice = Handshake.secureHandshake @ByteString skAlice infoAlice netAlice
    let hsBob   = Handshake.secureHandshake @ByteString skBob   infoBob   netBob

    hrs <- runHandshake infoAlice hsAlice hsBob
    case hrs of
        ( Left (Handshake.SimpleHandshakeError Handshake.NetworkMismatch),
          Left (Handshake.SimpleHandshakeError Handshake.NetworkMismatch))
           -> success
        _  -> failure

prop_secureApiInfoExchange :: forall c. Dict (IsCrypto c) -> Property
prop_secureApiInfoExchange Dict = withTests 1 . property $ do
    network                            <- forAll genNetwork
    ((pkAlice,skAlice), (pkBob,skBob)) <- genKeyPairPair @c

    infoAlice <- forAll $ genNodeInfo pkAlice
    infoBob   <- forAll $ genNodeInfo pkBob

    let hsAlice   = Handshake.secureHandshake @LByteString skAlice infoAlice network
    let hsBob     = Handshake.secureHandshake @LByteString skBob   infoBob   network

    hrs <- runHandshake infoAlice hsAlice hsBob
    case hrs of
        (Left e, _) -> annotateShow e *> failure
        (_, Left e) -> annotateShow e *> failure
        (Right hrAlice, Right hrBob) -> do
            -- Alice and Bob know the other's NodeInfo info, respectively
            Handshake.hrPeerInfo hrAlice === infoBob
            Handshake.hrPeerInfo hrBob   === infoAlice

--------------------------------------------------------------------------------

runHandshake
    :: (MonadTest m, MonadIO m)
    => nid
    -> (HandshakeRole -> Maybe nid -> HandshakeT e IO a)
    -> (HandshakeRole -> Maybe nid -> HandshakeT e IO a)
    -> m (Either e a, Either e a)
runHandshake nidA hsA hsB = do
    ((ttA, closeA), (ttB, closeB)) <- liftIO framedPair
    evalIO $ concurrently
        (Handshake.runHandshakeT ttA (hsA Handshake.Acceptor Nothing)
            `finally` closeA)
        (Handshake.runHandshakeT ttB (hsB Handshake.Connector (Just nidA))
            `finally` closeB)

tripping' :: (MonadTest m, Eq a, Show a) => a -> (a -> m b) -> (b -> m a) -> m ()
tripping' x f g = do
    fx <- f x
    gf <- g fx
    gf === x

nidHash
    :: Crypto.Hashable c (PublicKey c)
    => NodeInfo c
    -> Crypto.Hashed c (PublicKey c)
nidHash = Crypto.hash . fromNodeId . nodeNodeId

condensedS :: Condensed a => a -> String
condensedS = toS . condensed

genNodeInfo :: PublicKey c -> Gen (NodeInfo c)
genNodeInfo pk = do
    apiAddr <- genAddr
    pure $ mkNodeInfo apiAddr (mkNodeId pk)

genKeyPair
    :: forall c.
    ( HasHashing            c
    , HasDigitalSignature   c
    , Condensed (PublicKey  c)
    , Condensed (PrivateKey c)
    )
    => PropertyT IO (KeyPair c)
genKeyPair =
    forAllWith condensedS (quickcheck (arbitraryKeyPairs @c 1)) >>= \case
        [a] -> pure a
        x   -> annotate (condensedS x) *> failure

genAddr
    :: Gen Addr
genAddr = do
    host <- genHost
    port <- fromIntegral <$> Gen.word16 Range.constantBounded
    pure $ mkAddr host port

genKeyPairPair
    :: forall c.
    ( HasHashing            c
    , HasDigitalSignature   c
    , Condensed (PublicKey  c)
    , Condensed (PrivateKey c)
    )
    => PropertyT IO (KeyPair c, KeyPair c)
genKeyPairPair =
    forAllWith condensedS (quickcheck (arbitraryKeyPairs @c 2)) >>= \case
        [a, b] -> pure (a, b)
        x      -> annotate (condensedS x) *> failure
