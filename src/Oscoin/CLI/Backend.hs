module Oscoin.CLI.Backend where

import           Oscoin.Prelude

import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.Revision

import           Oscoin.Crypto.PubKey (PublicKey, PrivateKey)

data Backend a = Backend
    { revisionCreate           :: Revision -> (PublicKey, PrivateKey) -> IO (Result a)
    , revisionList             :: IO (Result a)
    , revisionStatus           :: RevisionId -> IO (Result a)
    , revisionMerge            :: RevisionId -> IO (Result a)
    , revisionSuggest          :: RevisionId -> Suggestion -> IO (Result a)
    }
