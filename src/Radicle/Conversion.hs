{-# OPTIONS_GHC -fno-warn-orphans #-}
module Radicle.Conversion
    ( (.:)
    , Dict
    , withKeywordDict

    , parseRadicleKeyword

    , module Radicle
    ) where

import           Oscoin.Prelude

import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Radicle (FromRad(..), ToRad(..))
import qualified Radicle.Extended as Rad

type Result a = Either Text a
type Dict = Map.Map Text Rad.Value

(.:) :: FromRad a => Dict -> Text -> Result a
(.:) dict key =
    case Map.lookup key dict of
        Just v  -> fromRad v
        Nothing -> throwError $ "Key \"" <> key <> "\" not found"

-- | Parse a radical value as a dictionary with keywords as keys and
-- apply the given function to the dictionary. If the value is not a
-- dictionary, 'typeMismatch' is called with the first argument.
withKeywordDict :: Text -> (Dict -> Result a) -> Rad.Value -> Result a
withKeywordDict _ k (Rad.Dict dict) =
    k =<< Map.foldlWithKey addKeywordItem (pure Map.empty) dict
  where
    addKeywordItem newDict key value =
        Map.insert <$> parseRadicleKeyword key <*> pure value <*> newDict
withKeywordDict expected _ value = typeMismatch expected value

typeMismatch :: Text -> Rad.Value -> Result a
typeMismatch expected actual =
    throwError $ "expected " <> expected <> ", encountered " <> name
  where
    name = case actual of
             Rad.Atom _    -> "Atom"
             Rad.Boolean _ -> "Boolean"
             Rad.Dict _    -> "Dict"
             Rad.Keyword _ -> "Keyword"
             Rad.Lambda{}  -> "Lambda"
             Rad.List _    -> "List"
             Rad.Number _  -> "Number"
             Rad.PrimFn _  -> "Primop"
             Rad.Ref _     -> "Ref"
             Rad.String _  -> "String"
             Rad.Vec _     -> "Vec"

instance ToRad a => ToRad (Set a) where
    toRad = toRad . Set.toList

instance (FromRad a, Ord a) => FromRad (Set a) where
    fromRad val = Set.fromList <$> fromRad val

parseRadicleKeyword :: Rad.Value -> Result Text
parseRadicleKeyword (Rad.Keyword (Rad.Identifier i)) = pure i
parseRadicleKeyword value                            = typeMismatch "Keyword" value
