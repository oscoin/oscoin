module Oscoin.CLI
    ( module Oscoin.CLI.Command
    , module Oscoin.CLI.Parser
    , CLI(..)
    , CommandRunner
    , runCommand
    ) where

import qualified Oscoin.API.Client as API
import           Oscoin.API.Client.HTTP (createNetworkHttpClient)
import           Oscoin.CLI.Command
import           Oscoin.CLI.KeyStore
import           Oscoin.CLI.Parser (CLI(..), execParser, execParserPure)
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Orphans ()
import           Oscoin.Prelude
import qualified Oscoin.Time as Time

import           Control.Concurrent (threadDelay)
import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS

type CommandRunner a = CommandRunnerT IO a

runCommand :: Maybe FilePath -> Command Crypto -> IO Result
runCommand mbKeysPath cmd = do
    commandClient <- createNetworkHttpClient "http://127.0.0.1:8477"
    let commandEnv = CommandEnv{commandClient}
    flip runReaderT mbKeysPath $
        flip runReaderT commandEnv $ runCommandRunnerT $ dispatchCommand cmd

data CommandEnv = CommandEnv { commandClient :: API.Client Crypto IO  }

newtype CommandRunnerT m a = CommandRunnerT { runCommandRunnerT :: ReaderT CommandEnv m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadMask
             , MonadCatch
             , MonadThrow
             , MonadTrans
             )

askCommandEnv :: Monad m => CommandRunnerT m CommandEnv
askCommandEnv = CommandRunnerT ask

instance MonadKeyStore Crypto m => MonadKeyStore Crypto (CommandRunnerT m)

instance MonadRandom m => MonadRandom (CommandRunnerT m) where
    getRandomBytes = lift . getRandomBytes

instance
    ( MonadKeyStore Crypto m
    , MonadIO m
    , MonadRandom m
    ) => MonadCLI Crypto (CommandRunnerT m)
  where
    sleep milliseconds = liftIO $ threadDelay (1000 * milliseconds)
    putLine            = liftIO . putStrLn
    putString          = liftIO . putStr
    readTxFile path    =
        either (panic . toS) identity . JSON.eitherDecode <$>
            liftIO (LBS.readFile path)
    getTime            = liftIO Time.now
    getClient          = API.hoistClient liftIO . commandClient <$> askCommandEnv
