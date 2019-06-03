-- | The 'MonadKeyStore' type class provides an interface to store and
-- retrieve a key pair. The instance for IO which is used by the CLI
-- uses the file system for persistence and stores the keys in
-- @~/.config/oscoin/id.{key,pub}`@. We use 'getXdgDirectory' to
-- deterimine the directory to store keys in.
module Oscoin.CLI.KeyStore
    ( MonadKeyStore(..)
    ) where

import           Oscoin.Crypto
import           Oscoin.Prelude

import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import qualified Oscoin.Crypto.PubKey.FileStore as Crypto

import           System.Directory
import           System.FilePath


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

instance MonadKeyStore Crypto (ReaderT (Maybe FilePath) IO) where
    keysPath = ask
    writeKeyPair keyPair =  do
        mbUserPath <- keysPath
        keyDir <- getConfigPath mbUserPath
        liftIO $ createDirectoryIfMissing True keyDir
        let keyPrefix = keyDir </> "id"
        Crypto.writeKeyPair keyPrefix keyPair

    readKeyPair = do
        mbUserPath <- keysPath
        keyDir <- getConfigPath mbUserPath
        let keyPrefix = keyDir </> "id"
        Crypto.readKeyPair keyPrefix
