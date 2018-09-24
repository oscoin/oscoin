module Oscoin.Git.Remote.Helper
    ( runRemoteHelper
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Client as API
import           Oscoin.API.HTTP.Client (HttpClientT, runHttpClientT)
import           Oscoin.CLI.KeyStore (MonadKeyStore)

import           Crypto.Random.Types (MonadRandom(..))

-- | The remote helper monad stack.
newtype HelperT m a = HelperT { runHelperT :: HttpClientT m a }
    deriving (Functor, Applicative, Monad, MonadTrans, API.MonadClient)

instance MonadKeyStore m => MonadKeyStore (HelperT m)

instance MonadRandom m => MonadRandom (HelperT m) where
    getRandomBytes = lift . getRandomBytes

--------------------------------------------------------------------------------

runRemoteHelper :: IO ()
runRemoteHelper =
    runHttpClientT nodeAddr $ runHelperT $ remoteHelper
  where
    nodeAddr = "http://127.0.0.1:8080"

remoteHelper :: API.MonadClient m => m ()
remoteHelper = pure ()
