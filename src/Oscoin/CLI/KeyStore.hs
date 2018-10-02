-- | The 'MonadKeyStore' type class provides an interface to store and
-- retrieve a key pair. The instance for IO which is used by the CLI
-- uses the file system for persistence and stores the keys in
-- @~/.config/oscoin/id.{key,pub}`@. We use 'getXdgDirectory' to
-- deterimine the directory to store keys in.
module Oscoin.CLI.KeyStore
    ( MonadKeyStore(..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Crypto.PubKey as Crypto

import           Codec.Serialise (deserialiseOrFail, serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           System.Directory
import           System.FilePath


class Monad m => MonadKeyStore m where
    writeKeyPair :: Crypto.KeyPair -> m ()

    default writeKeyPair
        :: (MonadKeyStore m', MonadTrans t, m ~ t m')
        => Crypto.KeyPair -> m ()
    writeKeyPair = lift . writeKeyPair

    readKeyPair :: m Crypto.KeyPair
    default readKeyPair
        :: (MonadKeyStore m', MonadTrans t, m ~ t m')
        => m Crypto.KeyPair
    readKeyPair = lift readKeyPair


getConfigPath :: MonadIO m => FilePath -> m FilePath
getConfigPath path = liftIO $ getXdgDirectory XdgConfig $ "oscoin" </> path

getSecretKeyPath :: IO FilePath
getSecretKeyPath = getConfigPath "id.key"

getPublicKeyPath :: IO FilePath
getPublicKeyPath = getConfigPath "id.pub"

ensureConfigDir :: MonadIO m => m ()
ensureConfigDir = do
    configDir <- getConfigPath ""
    liftIO $ createDirectoryIfMissing True configDir

instance MonadKeyStore IO where
    writeKeyPair (pk, sk) =  do
        ensureConfigDir
        skPath <- getSecretKeyPath
        LBS.writeFile skPath $ Crypto.serialisePrivateKey sk
        pkPath <- getPublicKeyPath
        LBS.writeFile pkPath $ serialise pk

    readKeyPair = do
        skPath <- getSecretKeyPath
        sk <- fromRightThrow =<< Crypto.deserialisePrivateKey <$> readFileLbs skPath
        pkPath <- getPublicKeyPath
        pk <- fromRightThrow =<< deserialiseOrFail <$> readFileLbs pkPath
        pure (pk, sk)
      where
        fromRightThrow = either throwM pure
        -- Avoiding lazy IO
        readFileLbs path = LBS.fromStrict <$> BS.readFile path
