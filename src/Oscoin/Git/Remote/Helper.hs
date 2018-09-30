module Oscoin.Git.Remote.Helper
    ( runRemoteHelper
    ) where

import           Oscoin.Prelude

import           Crypto.Random.Types (MonadRandom(..))
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Text as T
import qualified Oscoin.API.Client as API
import           Oscoin.API.HTTP.Client (HttpClientT, runHttpClientT)
import           Oscoin.CLI.KeyStore (MonadKeyStore)
import           System.Process

-- | The remote helper monad stack.
newtype HelperT m a = HelperT { runHelperT :: HttpClientT m a }
    deriving (Functor, Applicative, Monad, MonadTrans, API.MonadClient)

instance MonadKeyStore m => MonadKeyStore (HelperT m)

instance MonadRandom m => MonadRandom (HelperT m) where
    getRandomBytes = lift . getRandomBytes

--------------------------------------------------------------------------------

-- TODO: detect if it's fetch or push and execute appropriate node operations

runRemoteHelper :: IO ()
runRemoteHelper = do
    [remoteName, rawUrl] <- getArgs
    remoteHelper remoteName $ url rawUrl
  where
    url = fromJust . L.stripPrefix "oss://"
    nodeAddr = "http://127.0.0.1:8080"

remoteHelper :: String -> String -> IO ()
remoteHelper remoteName url = do
    -- TODO: map protocol to git-remote-<PROTOCOL-HANDLER>
    createProcess (proc "/usr/lib/git-core/git-remote-https"
                         [remoteName, url])
         { std_in = Inherit }
      >> pure ()
