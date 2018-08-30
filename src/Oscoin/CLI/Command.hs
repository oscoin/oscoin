module Oscoin.CLI.Command
    ( Command(..)
    , runCommand

    , Options(..)
    , defaultOptions
    ) where

import           Oscoin.Prelude

import           Oscoin.CLI.Backend
import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.Revision

import           Oscoin.Crypto.PubKey

data Command =
      RevisionCreate
    | RevisionList
    | RevisionStatus
    deriving (Show)

instance Read Command where
    readsPrec _ "create" = [(RevisionCreate, "")]
    readsPrec _ "list"   = [(RevisionList, "")]
    readsPrec _ "status" = [(RevisionStatus, "")]
    readsPrec _ cmd      = error $ "Invalid command '" ++ cmd ++ "'"

data Options = Options
    { optsRevisionId :: Maybe RevisionId
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
    { optsRevisionId = Nothing
    }

runCommand :: Backend a -> Command -> Options -> IO (Result a)
runCommand Backend{..} RevisionCreate _opts = do
    pair <- generateKeyPair
    revisionCreate emptyRevision pair
runCommand Backend{..} RevisionList _opts =
    notImplemented
runCommand Backend{..} RevisionStatus _opts =
    notImplemented
