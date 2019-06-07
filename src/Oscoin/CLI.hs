module Oscoin.CLI
    ( module Oscoin.CLI.Command
    , module Oscoin.CLI.Parser
    , CLI(..)
    , CommandRunner
    , runCommand
    ) where

import           Oscoin.CLI.Command
import           Oscoin.CLI.KeyStore
import           Oscoin.CLI.Parser (CLI(..), execParser, execParserPure)
import           Oscoin.Crypto (Crypto)
import           Oscoin.Crypto.Orphans ()
import           Oscoin.Prelude
import qualified Oscoin.Time as Time

import           Control.Concurrent (threadDelay)
import           Crypto.Random.Types (MonadRandom(..))

type CommandRunner a = CommandRunnerT IO a

runCommand :: Maybe FilePath -> Command Crypto -> IO Result
runCommand mbKeysPath cmd =
    flip runReaderT mbKeysPath $
        runCommandRunnerT $ dispatchCommand cmd

newtype CommandRunnerT m a = CommandRunnerT { runCommandRunnerT :: m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadMask
             , MonadCatch
             , MonadThrow
             )

instance MonadTrans CommandRunnerT where
    lift = CommandRunnerT

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
    getTime            = liftIO Time.now
