module Oscoin.CLI.Backend where

import           Oscoin.Prelude

import           Oscoin.CLI.Command.Result
import           Oscoin.CLI.Revision

data Backend a = Backend
    { revisionCreate           :: Revision -> IO (Result a)
    , revisionList             :: IO (Result a)
    , revisionStatus           :: RevisionId -> IO (Result a)
    , revisionMerge            :: RevisionId -> IO (Result a)
    , revisionSuggest          :: RevisionId -> Suggestion -> IO (Result a)
    }
