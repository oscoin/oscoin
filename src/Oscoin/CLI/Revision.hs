module Oscoin.CLI.Revision where

import           Oscoin.Prelude

import           Oscoin.CLI.Changeset
import           Oscoin.CLI.User

import           Radicle.Conversion
import qualified Radicle.Extended as Rad

import qualified Data.Map as Map

newtype RevisionId = RevisionId { fromRevisionId :: Int }
    deriving (Show, Num, Eq, Read, ToRadicle, FromRadicle)

data RevisionStatus =
      RevisionOpen
    | RevisionClosed
    | RevisionMerged
    deriving (Show, Eq)

instance ToRadicle RevisionStatus where
    toRadicle RevisionOpen   = Rad.keyword "open"
    toRadicle RevisionClosed = Rad.keyword "closed"
    toRadicle RevisionMerged = Rad.keyword "merged"

instance FromRadicle RevisionStatus where
    parseRadicle val = do
        kw <- parseRadicleKeyword val
        case kw of
            "open"   -> pure $ RevisionOpen
            "closed" -> pure $ RevisionClosed
            "merged" -> pure $ RevisionMerged
            _        -> Left $ "Unmatched revision status \"" <> kw <> "\""

type Ref = Text

newtype RevisionBase = RevisionBase Ref
    deriving (Show, Eq, IsString, ToRadicle, FromRadicle)

data Revision = Revision
      { revId          :: RevisionId
      , revTitle       :: Text
      , revDescription :: Text
      , revChangeset   :: Changeset
      , revAuthor      :: User
      , revReviewers   :: Set User
      , revStatus      :: RevisionStatus
      , revBase        :: RevisionBase
      } deriving (Show)

instance ToRadicle Revision where
    toRadicle Revision{..} =
        Rad.Dict $ Map.fromList $ map (first Rad.keyword) $
            [ ("id",          toRadicle revId)
            , ("title",       toRadicle revTitle)
            , ("description", toRadicle revDescription)
            , ("changeset",   toRadicle revChangeset)
            , ("author",      toRadicle revAuthor)
            , ("reviewers",   toRadicle revReviewers)
            , ("status",      toRadicle revStatus)
            , ("base",        toRadicle revBase)
            ]
instance FromRadicle Revision where
    parseRadicle =
        withKeywordDict "Revision" $ \d ->
            Revision
                <$> d .: "id"
                <*> d .: "title"
                <*> d .: "description"
                <*> d .: "changeset"
                <*> d .: "author"
                <*> d .: "reviewers"
                <*> d .: "status"
                <*> d .: "base"

mkRevision :: RevisionId -> Text -> Text -> Changeset -> User -> RevisionBase -> Revision
mkRevision id title desc changes author base =
    Revision
        { revId = id
        , revTitle = title
        , revDescription = desc
        , revChangeset = changes
        , revAuthor = author
        , revReviewers = mempty
        , revStatus = RevisionOpen
        , revBase = base
        }

emptyRevision :: Revision
emptyRevision = mkRevision 0 "" "" mempty "anonymous" "master"

newtype SuggestionId = SuggestionId Int
    deriving (Show, Eq, Read)

data Suggestion = Suggestion SuggestionId Text
    deriving (Show)
