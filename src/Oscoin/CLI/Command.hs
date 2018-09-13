module Oscoin.CLI.Command
    ( Command(..)
    , runCommand
    ) where

import           Oscoin.Prelude

import qualified Oscoin.API.Types as API
import qualified Oscoin.API.Client as API
import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.Revision
import qualified Oscoin.CLI.Radicle as Rad
import           Oscoin.Crypto.PubKey (sign, generateKeyPair)
import           Oscoin.Data.Tx (mkTx)


data Command =
      RevisionCreate
    | RevisionList
    | RevisionStatus
    deriving (Show)


runCommand :: (MonadIO m, API.MonadClient m) => Command -> m (Result Text)
runCommand RevisionCreate = do
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
        pure $ mkTx msg pk


runCommand _ = notImplemented
