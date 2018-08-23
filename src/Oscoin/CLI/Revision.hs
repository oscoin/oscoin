module Oscoin.CLI.Revision where

import           Oscoin.Prelude

import           Oscoin.CLI.Changeset
import           Oscoin.CLI.Radicle
import           Oscoin.CLI.User

import qualified Radicle as Rad

import qualified Data.Map as Map

newtype RevisionId = RevisionId { fromRevisionId :: Int }
    deriving (Show, Eq, Read, ToRadicle)

data RevisionStatus =
      RevisionOpen
    | RevisionClosed
    | RevisionMerged
    deriving (Show, Eq)

instance ToRadicle RevisionStatus where
    toRadicle rs = case rs of
        RevisionOpen     -> key "open"
        RevisionClosed   -> key "closed"
        RevisionMerged   -> key "merged"
      where
        key = Rad.Keyword . fromJust . Rad.mkIdent

data Revision = Revision
      { revId          :: RevisionId
      , revTitle       :: Text
      , revDescription :: Text
      , revChangeset   :: Changeset
      , revAuthor      :: User
      , revReviewers   :: Set User
      , revStatus      :: RevisionStatus
      } deriving (Show)

instance ToRadicle Revision where
    toRadicle Revision{..} =
        Rad.Dict $ Map.fromList $ map (first Rad.String) $
            [ ("id",          toRadicle revId)
            , ("title",       toRadicle revTitle)
            , ("description", toRadicle revDescription)
            , ("changeset",   toRadicle revChangeset)
            , ("author",      toRadicle revAuthor)
            , ("reviewers",   toRadicle revReviewers)
            , ("status",      toRadicle revStatus)
            ]

mkRevision :: RevisionId -> Text -> Text -> Changeset -> User -> Revision
mkRevision id title desc changes author =
    Revision
        { revId = id
        , revTitle = title
        , revDescription = desc
        , revChangeset = changes
        , revAuthor = author
        , revReviewers = mempty
        , revStatus = RevisionOpen
        }

newtype SuggestionId = SuggestionId Int
    deriving (Show, Eq, Read)

data Suggestion = Suggestion SuggestionId Text
    deriving (Show)
