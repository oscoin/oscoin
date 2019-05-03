-- | The 'MonadKeyStore' type class provides an interface to store and
-- retrieve a key pair. The instance for IO which is used by the CLI
-- uses the file system for persistence and stores the keys in
-- @~/.config/oscoin/id.{key,pub}`@. We use 'getXdgDirectory' to
-- deterimine the directory to store keys in.
module Oscoin.CLI.KeyStore
    ( MonadKeyStore(..)
    , KeyStoreError(..)
    ) where

import           Oscoin.Crypto
import           Oscoin.Prelude
import qualified Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Crypto.PubKey.RealWorld as Crypto.RealWorld

import           Codec.Serialise
                 (DeserialiseFailure, deserialiseOrFail, serialise)
import           Control.Monad.Except (liftEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           System.Directory
import           System.FilePath
import           System.IO.Error (isDoesNotExistError)


class (Crypto.HasHashing c, Monad m) => MonadKeyStore c m | m -> c where

    keysPath     :: m (Maybe FilePath)

    default keysPath
        :: (MonadKeyStore c m', MonadTrans t, m ~ t m')
        => m (Maybe FilePath)
    keysPath = lift keysPath

    writeKeyPair :: Crypto.KeyPair c -> m ()

    default writeKeyPair
        :: (MonadKeyStore c m', MonadTrans t, m ~ t m')
        => Crypto.KeyPair c -> m ()
    writeKeyPair = lift . writeKeyPair

    readKeyPair :: m (Crypto.KeyPair c)
    default readKeyPair
        :: (MonadKeyStore c m', MonadTrans t, m ~ t m')
        => m (Crypto.KeyPair c)
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

data KeyStoreError =
    -- | Key file at specified path doesn't exist.
      KeyNotFound FilePath
    -- | A key couldn't be deserialised correctly. The first
    -- argument is the path to the key on disk.
    | KeyDeserialisationFailure FilePath DeserialiseFailure

instance Show KeyStoreError where
    show = \case
        KeyNotFound configPath ->
            "Key file doesn't exist: " <> configPath

        KeyDeserialisationFailure configPath cborError ->
            mconcat [ "Loading the key from "
                    , configPath
                    , " failed. The reason was: "
                    , show cborError
                    ]

instance Exception KeyStoreError

instance MonadKeyStore Crypto (ReaderT (Maybe FilePath) IO) where
    keysPath = ask
    writeKeyPair (pk, sk) =  do
        mbUserPath <- keysPath
        ensureConfigDir mbUserPath
        liftIO $ do
            skPath <- getSecretKeyPath mbUserPath
            LBS.writeFile skPath $ Crypto.RealWorld.serialisePrivateKey sk
            pkPath <- getPublicKeyPath mbUserPath
            LBS.writeFile pkPath $ serialise pk

    readKeyPair = do
        mbUserPath <- keysPath
        liftIO $ do
            skPath <- getSecretKeyPath mbUserPath
            pkPath <- getPublicKeyPath mbUserPath
            keys   <-
                runExceptT $ liftA2 (,)
                    (readFileLbs pkPath >>=
                        liftEither
                            . first (KeyDeserialisationFailure pkPath)
                            . deserialiseOrFail)
                    (readFileLbs skPath >>=
                        liftEither
                            . first (KeyDeserialisationFailure skPath)
                            . Crypto.RealWorld.deserialisePrivateKey)
            either throwM pure keys
      where
        readFileLbs path = ExceptT $
            map (first . const $ KeyNotFound path)
                . tryJust (guard . isDoesNotExistError)
                $ LBS.fromStrict <$> BS.readFile path
