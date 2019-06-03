-- | This module provides the 'writeKeyPair' and 'readKeyPair'
-- functions to read and write real world 'Crypto' keypairs to disk.
module Oscoin.Crypto.PubKey.FileStore
    ( writeKeyPair
    , readKeyPair
    , ReadKeyError(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto (Crypto)
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Crypto.PubKey.RealWorld as Crypto.RealWorld

import           Codec.Serialise
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           System.Directory
import           System.FilePath
import           System.IO.Error (isDoesNotExistError)


-- | Read a keypair from disk. See 'writeKeyPair' about how the path
-- parameter determines the key location. May raise 'ReadKeyError'.
readKeyPair :: (MonadIO m) => FilePath -> m (Crypto.KeyPair Crypto)
readKeyPair keysPathPrefix = liftIO $ do
    let (pkPath, skPath) = keyPaths keysPathPrefix
    pk <- readKeyFile deserialiseOrFail pkPath
    sk <- readKeyFile Crypto.RealWorld.deserialisePrivateKey skPath
    pure (pk ,sk)
  where
    readKeyFile customDeserialiseOrFail path = do
        contents <- readFileLbs path
        case customDeserialiseOrFail contents of
            Left err  -> throw $ KeyDeserialisationFailure path err
            Right key -> pure key
    readFileLbs path = do
        contentsE <- tryJust (guard . isDoesNotExistError) $ BS.readFile path
        case contentsE of
            Left _         -> throw $ KeyNotFound path
            Right contents -> pure $ LBS.fromStrict contents


data ReadKeyError =
    -- | Key file at specified path doesn't exist.
      KeyNotFound FilePath
    -- | A key couldn't be deserialised correctly. The first
    -- argument is the path to the key on disk.
    | KeyDeserialisationFailure FilePath DeserialiseFailure
    deriving (Eq, Show)

instance Exception ReadKeyError where
    displayException = \case
        KeyNotFound configPath ->
            "Key file doesn't exist: " <> configPath

        KeyDeserialisationFailure configPath cborError ->
            mconcat [ "Loading the key from "
                    , configPath
                    , " failed. The reason was: "
                    , show cborError
                    ]


-- | Write a keypair to disk at the given location. The public key path
-- is obtained by appending @".pub"@ to the given path and the private
-- key path is obtained by appending @".id"@.
writeKeyPair :: (MonadIO m) => FilePath -> Crypto.KeyPair Crypto -> m ()
writeKeyPair keysPathPrefix (pk, sk) = liftIO $ do
    let keyDir = takeDirectory keysPathPrefix
    let (pkPath, skPath) = keyPaths keysPathPrefix
    createDirectoryIfMissing True keyDir
    LBS.writeFile pkPath $ serialise pk
    LBS.writeFile skPath $ Crypto.RealWorld.serialisePrivateKey sk

-- Internal -------------------------------------------------------

-- | Given a key path prefix returns the paths of the public key and
-- private key.
--
-- >>> keyPaths "foo/bar"
-- ("foo/bar.pub", "foo/bar.key")
--
keyPaths :: FilePath -> (FilePath, FilePath)
keyPaths keysPathPrefix =
    ( keysPathPrefix <> ".pub"
    , keysPathPrefix <> ".key"
    )
