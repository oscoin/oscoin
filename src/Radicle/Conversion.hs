-- | Provide 'FromRadicle' and 'ToRadicle' instances in the spirit of
-- 'Data.Aeson.FromJSON' and 'Data.Aeson.ToJSON'.
--
-- Most of the provided functions closely match those from
-- "Data.Aeson".
module Radicle.Conversion
    ( ToRadicle(..)
    , FromRadicle(..)

    , withKeywordDict
    , (.:)

    , parseRadicleKeyword
    , parseRadicleList
    , parseRadicleIntegral
    ) where

import           Oscoin.Prelude hiding (list)

import qualified Data.Map as Map
import           Data.Scientific
import qualified Data.Set as Set
import qualified Radicle.Extended as Rad

type Result a = Either Text a

type Dict = Map.Map Text Rad.Value

class ToRadicle a where
    toRadicle :: a -> Rad.Value

class FromRadicle a where
    parseRadicle :: Rad.Value -> Result a


instance ToRadicle Rad.Value where
    toRadicle = identity

instance FromRadicle Rad.Value where
    parseRadicle val = Right val


instance ToRadicle Int where
    toRadicle x = Rad.Number (scientific (toInteger x) 0)

instance FromRadicle Int where
    parseRadicle = parseRadicleIntegral


instance ToRadicle Text where
    toRadicle = Rad.String

instance FromRadicle Text where
    parseRadicle (Rad.String t) = pure t
    parseRadicle val            = typeMismatch "String" val


instance ToRadicle () where
    toRadicle _ = Rad.list $ []

instance FromRadicle () where
    parseRadicle val = do
        l <- parseRadicleList val
        case l of
            [] -> pure ()
            _  -> throwError "Expected list to be empty"


instance ToRadicle a => ToRadicle [a] where
    toRadicle = Rad.list . map toRadicle

instance (FromRadicle a) => FromRadicle [a] where
    parseRadicle val = parseRadicleList val >>= traverse parseRadicle


instance ToRadicle a => ToRadicle (Set a) where
    toRadicle = toRadicle . Set.toList

instance (FromRadicle a, Ord a) => FromRadicle (Set a) where
    parseRadicle val = Set.fromList <$> parseRadicle val


instance ToRadicle Scientific where
    toRadicle = Rad.Number

instance FromRadicle Scientific where
    parseRadicle (Rad.Number n) = pure n
    parseRadicle value          = typeMismatch "Number" value

------------------------------------------------------------------------

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

parseRadicleIntegral :: (Integral a, Bounded a) => Rad.Value -> Result a
parseRadicleIntegral val = do
    n :: Scientific <- parseRadicle val
    case toBoundedInteger n of
        Just n' -> pure n'
        Nothing -> throwError "Number is not integral or out of bounds"

parseRadicleKeyword :: Rad.Value -> Result Text
parseRadicleKeyword (Rad.Keyword (Rad.Ident i)) = pure i
parseRadicleKeyword value                       = typeMismatch "Keyword" value

-- | Lookup a value in a dictionary and convert it. Fails if the key is
-- not found.
(.:) :: FromRadicle a => Dict -> Text -> Result a
(.:) dict key =
    case Map.lookup key dict of
        Just v  -> parseRadicle v
        Nothing -> throwError $ "Key \"" <> key <> "\" not found"

parseRadicleList :: Rad.Value -> Result [Rad.Value]
parseRadicleList val
    | (Rad.List (x:xs)) <- val
    , x == Rad.listPrimop
    = pure xs
    | otherwise
    = throwError "Expected unevaluated list"

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
             Rad.Primop _  -> "Primop"
             Rad.Ref _     -> "Ref"
             Rad.String _  -> "String"
