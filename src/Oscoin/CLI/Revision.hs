module Oscoin.CLI.Revision where

import           Oscoin.Prelude

import           Oscoin.CLI.Changeset
import           Oscoin.CLI.User

import           Radicle.Conversion
import qualified Radicle.Extended as Rad

import qualified Data.Map as Map

newtype RevisionId = RevisionId { fromRevisionId :: Integer }
    deriving (Show, Num, Eq, Read, ToRad, FromRad)

data RevisionStatus =
      RevisionOpen
    | RevisionClosed
    | RevisionMerged
    deriving (Show, Eq)

instance ToRad RevisionStatus where
    toRad RevisionOpen   = Rad.keyword "open"
    toRad RevisionClosed = Rad.keyword "closed"
    toRad RevisionMerged = Rad.keyword "merged"

instance FromRad RevisionStatus where
    fromRad val = do
        kw <- parseRadicleKeyword val
        case kw of
            "open"   -> pure $ RevisionOpen
            "closed" -> pure $ RevisionClosed
            "merged" -> pure $ RevisionMerged
            _        -> Left $ "Unmatched revision status \"" <> kw <> "\""

type Ref = Text

newtype RevisionBase = RevisionBase Ref
    deriving (Show, Eq, IsString, ToRad, FromRad)

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

instance ToRad Revision where
    toRad Revision{..} =
        Rad.annotate $ Rad.DictF $ Map.fromList $ map (first Rad.keyword) $
            [ ("id",          toRad revId)
            , ("title",       toRad revTitle)
            , ("description", toRad revDescription)
            , ("changeset",   toRad revChangeset)
            , ("author",      toRad revAuthor)
            , ("reviewers",   toRad revReviewers)
            , ("status",      toRad revStatus)
            , ("base",        toRad revBase)
            ]
instance FromRad Revision where
    fromRad =
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
