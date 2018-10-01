module Oscoin.Git.Remote.Helper
    ( runRemoteHelper
    ) where

import           Oscoin.Prelude

import qualified Data.Text as T
import           System.Process

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

remoteHelper :: Text -> Text -> IO ()
remoteHelper remoteName url =
    -- TODO: map protocol to git-remote-<PROTOCOL-HANDLER>
    -- TODO: wait for process, once we do concurrent node ops
    createProcess (proc "/usr/lib/git-core/git-remote-https"
                         [T.unpack remoteName, T.unpack url])
         { std_in = Inherit }
      >> pure ()
