module Oscoin.Git.Remote.Helper
    ( runRemoteHelper
    ) where

import           Oscoin.Prelude

-- import           Crypto.Random.Types (MonadRandom(..))
-- import           Data.Maybe
import qualified Data.Text as T
-- import qualified Oscoin.API.Client as API
-- import           Oscoin.API.HTTP.Client (HttpClientT, runHttpClientT)
-- import           Oscoin.CLI.KeyStore (MonadKeyStore)
import           System.Process

{-
-- | The remote helper monad stack.
newtype HelperT m a = HelperT { runHelperT :: HttpClientT m a }
    deriving (Functor, Applicative, Monad, MonadTrans, API.MonadClient)

instance MonadKeyStore m => MonadKeyStore (HelperT m)

instance MonadRandom m => MonadRandom (HelperT m) where
    getRandomBytes = lift . getRandomBytes
-}

--------------------------------------------------------------------------------

-- TODO: detect if it's fetch or push and execute appropriate node
--       operations, probably by implementing a full proxy that relays the
--       protocol unmodified to git-remote-<PROTOCOL-HANDLER> while parsing
--       it here, too, to detect fetch/push.

runRemoteHelper :: IO ()
runRemoteHelper = do
    [remoteName, rawUrl] <- map T.pack <$> getArgs
    remoteHelper remoteName $ url rawUrl
  where
    url = T.replace "oss://" ""
    -- nodeAddr = "http://127.0.0.1:8080"

remoteHelper :: Text -> Text -> IO ()
remoteHelper remoteName url =
    -- TODO: map protocol to git-remote-<PROTOCOL-HANDLER>
    -- TODO: wait for process, once we do concurrent node ops
    createProcess (proc "/usr/lib/git-core/git-remote-https"
                         [T.unpack remoteName, T.unpack url])
         { std_in = Inherit }
      >> pure ()
