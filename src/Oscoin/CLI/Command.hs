module Oscoin.CLI.Command
    ( Command(..)
    , runCommand

    , Options(..)
    , defaultOptions
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import qualified Oscoin.API.Client as API
import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.Revision
import qualified Oscoin.CLI.Radicle as Rad
import           Oscoin.Crypto.PubKey (sign, generateKeyPair)
import           Oscoin.Crypto.Hash (hash)
import           Oscoin.Data.Tx (mkTx)


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

runCommand :: (MonadIO m, API.MonadClient m) => Command -> Options -> m (Result Text)
runCommand RevisionCreate _opts = do
    tx <- io createTransaction
    result <- API.submitTransaction tx
    pure $ case result of
        API.Ok v    -> ResultValue (tshow v)
        API.Err err -> ResultError err
  where
    createTransaction :: IO API.RadTx
    createTransaction = do
        (pk, sk) <- generateKeyPair
        let rev = emptyRevision
        let msgContent = Rad.fnApply "create-revision" [Rad.toRadicle rev]
        msg <- sign sk msgContent
        pure $ mkTx msg (hash pk)

runCommand _ _ = notImplemented
