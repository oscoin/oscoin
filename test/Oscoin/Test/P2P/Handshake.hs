module Oscoin.Test.P2P.Handshake (tests, props) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Crypto.PubKey as Crypto
import           Oscoin.P2P.Handshake as Handshake
import           Oscoin.P2P.Types (NodeId, fromNodeId, mkNodeId)

import           Oscoin.Test.Crypto
import           Oscoin.Test.P2P.Helpers (framedPair)

import qualified Crypto.Noise.Exception as Noise

import           Hedgehog
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

{-# ANN module ("HLint: ignore Reduce duplication" :: String ) #-}

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Handshake"
    [ testProperty "Simple Handshake" (testSimpleHandshake d)
    , testProperty "Secure Handshake" (testSecureHandshake d)
    ]

-- | For GHCi use.
props :: Dict (IsCrypto c) -> IO Bool
props d = checkParallel $ Group "P2P.Handshake"
    [ ("test_simple", testSimpleHandshake d)
    , ("test_secure", testSecureHandshake d)
    ]

testSimpleHandshake
    :: forall c. Dict (IsCrypto c)
    -> Property
testSimpleHandshake Dict = withTests 1 . property $ do
    ((pkAlice :: PK c, skAlice), (pkBob, skBob)) <- keyPairs
    ((ttAlice, close1), (ttBob, close2)) <- liftIO framedPair

    let nidAlice = mkNodeId pkAlice
    let nidBob   = mkNodeId pkBob
    let hsAlice = Handshake.simpleHandshake @ByteString (pkAlice, skAlice)
    let hBob    = Handshake.simpleHandshake @ByteString (pkBob,   skBob  )

    hrs <-
        liftIO $ concurrently
            (Handshake.runHandshakeT ttAlice
                (hsAlice Handshake.Acceptor Nothing) `finally` close1)
            (Handshake.runHandshakeT ttBob
                (hBob Handshake.Connector (Just nidAlice)) `finally` close2)

    case hrs of
        (Left _, _) -> failure
        (_, Left _) -> failure
        (Right hrAlice, Right hrBob) -> do
            -- Alice and Bob know the other's key, respectively
            nidHash (Handshake.hrPeerId hrAlice) === nidHash nidBob
            nidHash (Handshake.hrPeerId hrBob  ) === nidHash nidAlice

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

testSecureHandshake :: forall c. Dict (IsCrypto c) -> Property
testSecureHandshake Dict = withTests 1 . property $ do
    ((pkAlice :: PK c, skAlice), (pkBob, skBob)) <- keyPairs
    ((ttAlice, close1), (ttBob, close2)) <- liftIO framedPair

    let nidAlice = mkNodeId pkAlice
    let nidBob   = mkNodeId pkBob
    let hsAlice  = Handshake.secureHandshake @LByteString (pkAlice, skAlice)
    let hBob     = Handshake.secureHandshake @LByteString (pkBob,   skBob  )

    hrs <-
        liftIO $ concurrently
            (Handshake.runHandshakeT ttAlice
                (hsAlice Handshake.Acceptor Nothing) `finally` close1)
            (Handshake.runHandshakeT ttBob
                (hBob Handshake.Connector (Just nidAlice)) `finally` close2)

    case hrs of
        (Left _, _) -> failure
        (_, Left _) -> failure
        (Right hrAlice, Right hrBob) -> do
            -- Alice and Bob know the other's key, respectively
            nidHash (Handshake.hrPeerId hrAlice) === nidHash nidBob
            nidHash (Handshake.hrPeerId hrBob  ) === nidHash nidAlice

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

--------------------------------------------------------------------------------

tripping' :: (MonadTest m, Eq a, Show a) => a -> (a -> m b) -> (b -> m a) -> m ()
tripping' x f g = do
    fx <- f x
    gf <- g fx
    gf === x

keyPairs :: IsCrypto c => PropertyT IO (Crypto.KeyPair c, Crypto.KeyPair c)
keyPairs = liftA2 (,) gen gen
  where
    gen = evalIO Crypto.generateKeyPair

nidHash
    :: Crypto.Hashable c (PK c)
    => NodeId c
    -> Crypto.Hashed c (PK c)
nidHash = Crypto.hash . fromNodeId
