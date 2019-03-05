module Oscoin.CLI
    ( module Oscoin.CLI.Revision
    , module Oscoin.CLI.User
    , module Oscoin.CLI.Command
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
import           Oscoin.CLI.Revision
import qualified Oscoin.CLI.Spinner as Spinner
import           Oscoin.CLI.User
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Orphans ()
import           Oscoin.Prelude
import qualified Oscoin.Time as Time

import           Control.Concurrent (threadDelay)
import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.Text as T

import qualified Radicle.Extended as Rad

type CommandRunner a = CommandRunnerT IO a

runCommand :: Maybe FilePath -> Command -> IO Result
runCommand mbKeysPath cmd = flip runReaderT mbKeysPath $
    runHttpClientT "http://127.0.0.1:8477" $ runCommandRunnerT $ dispatchCommand cmd

newtype CommandRunnerT m a = CommandRunnerT { runCommandRunnerT :: HttpClientT m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadMask, MonadCatch, MonadThrow, MonadTrans, API.MonadClient Crypto)

instance MonadKeyStore Crypto m => MonadKeyStore Crypto (CommandRunnerT m)

instance MonadRandom m => MonadRandom (CommandRunnerT m) where
    getRandomBytes = lift . getRandomBytes

instance ( MonadKeyStore Crypto m
         , MonadIO m
         , MonadMask m
         , MonadRandom m
         ) => MonadCLI Crypto (CommandRunnerT m) where
    sleep milliseconds = liftIO $ threadDelay (1000 * milliseconds)
    putLine = liftIO . putStrLn
    putString = liftIO . putStr
    withSpinner = Spinner.withSpinner
    progress = Spinner.progress
    readRadFile path = do
        contents <- liftIO $ readFile path
        pure $ Rad.parse (T.pack path) contents
    getTime = liftIO Time.now
