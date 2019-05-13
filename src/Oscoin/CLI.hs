module Oscoin.CLI
    ( module Oscoin.CLI.Command
    , module Oscoin.CLI.Parser
    , CLI(..)
    , CommandRunner
    , runCommand
    ) where

import qualified Oscoin.API.Client as API
import           Oscoin.API.HTTP.Client (HttpClientT, runHttpClientT)
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
runCommand mbKeysPath cmd = flip runReaderT mbKeysPath $
    runHttpClientT "http://127.0.0.1:8477" $ runCommandRunnerT $ dispatchCommand cmd

newtype CommandRunnerT m a = CommandRunnerT { runCommandRunnerT :: HttpClientT m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadMask
             , MonadCatch
             , MonadThrow
             , MonadTrans
             , API.MonadClient Crypto
             )

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
