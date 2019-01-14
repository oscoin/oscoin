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

    keysPath     :: m (Maybe FilePath)

    default keysPath
        :: (MonadKeyStore m', MonadTrans t, m ~ t m')
        => m (Maybe FilePath)
    keysPath = lift keysPath

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


-- | Get the configuration path for the keys, but first looking at a
-- user-specified directory and falling back to the xdg directory otherwise.
getConfigPath :: MonadIO m => Maybe FilePath -> m FilePath
getConfigPath Nothing         = liftIO . getXdgDirectory XdgConfig $ "oscoin"
getConfigPath (Just userPath) = pure userPath

getSecretKeyPath :: Maybe FilePath -> IO FilePath
getSecretKeyPath mbFilePath = flip (</>) "id.key" <$> getConfigPath mbFilePath

getPublicKeyPath :: Maybe FilePath -> IO FilePath
getPublicKeyPath mbFilePath = flip (</>) "id.pub" <$> getConfigPath mbFilePath

ensureConfigDir :: MonadIO m => Maybe FilePath -> m ()
ensureConfigDir mbUserPath = do
    configDir <- getConfigPath mbUserPath
    liftIO $ createDirectoryIfMissing True configDir

instance MonadKeyStore (ReaderT (Maybe FilePath) IO) where
    keysPath = ask
    writeKeyPair (pk, sk) =  do
        mbUserPath <- keysPath
        ensureConfigDir mbUserPath
        liftIO $ do
            skPath <- getSecretKeyPath mbUserPath
            LBS.writeFile skPath $ Crypto.serialisePrivateKey sk
            pkPath <- getPublicKeyPath mbUserPath
            LBS.writeFile pkPath $ serialise pk

    readKeyPair = do
        mbUserPath <- keysPath
        liftIO $ do
            skPath <- getSecretKeyPath mbUserPath
            sk <- fromRightThrow =<< Crypto.deserialisePrivateKey <$> readFileLbs skPath
            pkPath <- getPublicKeyPath mbUserPath
            pk <- fromRightThrow =<< deserialiseOrFail <$> readFileLbs pkPath
            pure (pk, sk)
      where
        fromRightThrow = either throwM pure
        -- Avoiding lazy IO
        readFileLbs path = LBS.fromStrict <$> BS.readFile path
