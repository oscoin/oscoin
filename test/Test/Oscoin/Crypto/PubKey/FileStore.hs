module Test.Oscoin.Crypto.PubKey.FileStore
    ( tests
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.PubKey.FileStore
import           Test.Oscoin.Crypto.PubKey.Gen

import           System.FilePath
import           System.IO.Temp

import           Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Test.Oscoin.Crypto.PubKey.FileStore"
    [ testProperty "prop_writeReadTripping" $ prop_writeReadTripping
    , testProperty "prop_keyNotFoundError" $ prop_keyNotFoundError
    ]

prop_writeReadTripping :: Property
prop_writeReadTripping = property $ do
    kp <- forAllWith (const "<keypair>") genKeyPair
    kp' <- liftIO $ withSystemTempDirectory "oscoin-test" $ \tmpdir -> do
        let keyPrefix = tmpdir </> "id"
        writeKeyPair keyPrefix kp
        readKeyPair keyPrefix
    -- We don't use '(===)' because private keys don't have a 'Show'
    -- instance.
    assert $ kp == kp'

prop_keyNotFoundError :: Property
prop_keyNotFoundError = property $ do
    (result, keyPrefix) <- liftIO $ withSystemTempDirectory "oscoin-test" $ \tmpdir -> do
        let keyPrefix = tmpdir </> "id"
        result <- try $ readKeyPair keyPrefix
        pure (result, keyPrefix)
    void result === Left (KeyNotFound $ keyPrefix <> ".pub")
