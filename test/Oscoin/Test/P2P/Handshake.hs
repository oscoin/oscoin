module Oscoin.Test.P2P.Handshake (tests, props) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.P2P.Handshake as Handshake
import           Oscoin.P2P.Types (NodeId, fromNodeId, mkNodeId)

import           Oscoin.Test.P2P.Helpers (framedPair)

import qualified Crypto.Noise.Exception as Noise
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA

import           Hedgehog
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

{-# ANN module ("HLint: ignore Reduce duplication" :: String ) #-}

tests :: TestTree
tests = testGroup "Handshake"
    [ testProperty "Simple Handshake" testSimpleHandshake
    , testProperty "Secure Handshake" testSecureHandshake
    ]

-- | Gor GHCi use.
props :: IO Bool
props = checkParallel $ Group "P2P.Handshake"
    [ ("test_simple", testSimpleHandshake)
    , ("test_secure", testSecureHandshake)
    ]

testSimpleHandshake :: Property
testSimpleHandshake = withTests 1 . property $ do
    ((pkAlice, skAlice), (pkBob, skBob)) <- keyPairs
    ((ttAlice, close1), (ttBob, close2)) <- liftIO framedPair

    let nidAlice = mkNodeId pkAlice
    let nidBob   = mkNodeId pkBob
    let hsAlice  = Handshake.simpleHandshake @LByteString (pkAlice, skAlice)
    let hBob     = Handshake.simpleHandshake @LByteString (pkBob,   skBob  )

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

testSecureHandshake :: Property
testSecureHandshake = withTests 1 . property $ do
    ((pkAlice, skAlice), (pkBob, skBob)) <- keyPairs
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

keyPairs :: PropertyT IO (Crypto.KeyPair, Crypto.KeyPair)
keyPairs = liftA2 (,) gen gen
  where
    gen = evalIO Crypto.generateKeyPair

nidHash :: NodeId -> Crypto.Hashed ECDSA.PublicKey
nidHash = Crypto.publicKeyHash . fromNodeId
