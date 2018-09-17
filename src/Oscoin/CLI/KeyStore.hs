{-# LANGUAGE DefaultSignatures #-}
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

import           Codec.Serialise (serialise, deserialiseOrFail)
import           Control.Exception.Safe
import qualified Data.ByteString.Lazy as LBS
import           System.Directory
import           System.FilePath


class Monad m => MonadKeyStore m where
    writeKeyPair :: (Crypto.PublicKey, Crypto.PrivateKey) -> m ()

    default writeKeyPair
        :: (MonadKeyStore m', MonadTrans t, m ~ t m')
        => (Crypto.PublicKey, Crypto.PrivateKey) -> m ()
    writeKeyPair = lift . writeKeyPair

    readKeyPair :: m (Crypto.PublicKey, Crypto.PrivateKey)
    default readKeyPair
        :: (MonadKeyStore m', MonadTrans t, m ~ t m')
        => m (Crypto.PublicKey, Crypto.PrivateKey)
    readKeyPair = lift readKeyPair


getConfigPath :: MonadIO m => FilePath -> m FilePath
getConfigPath path = io $ getXdgDirectory XdgConfig $ "oscoin" </> path

ensureConfigDir :: MonadIO m => m ()
ensureConfigDir = do
    configDir <- getConfigPath ""
    io $ createDirectoryIfMissing True configDir

instance MonadKeyStore IO where
    writeKeyPair (pk, sk) =  do
        ensureConfigDir
        skPath <- getConfigPath "id.key"
        io $ LBS.writeFile skPath $ Crypto.serialisePrivateKey sk
        pkPath <- getConfigPath "id.pub"
        io $ LBS.writeFile pkPath (serialise pk)

    readKeyPair = do
        skPath <- getConfigPath "id.key"
        sk <- fromRightThrow =<< Crypto.deserialisePrivateKey <$> LBS.readFile skPath
        pkPath <- getConfigPath "id.pub"
        pk <- fromRightThrow =<< deserialiseOrFail <$> LBS.readFile pkPath
        pure (pk, sk)
      where
        fromRightThrow = either throwM pure