module Oscoin.CLI.Radicle
    ( ToRadicle(..)
    , FromRadicle(..)
    , fnApply
    ) where

import           Oscoin.Prelude

import           Data.Scientific (scientific)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Radicle as Rad

class ToRadicle a where
    toRadicle :: a -> Rad.Value

instance ToRadicle Int where
    toRadicle x = Rad.Number (scientific (toInteger x) 0)

instance ToRadicle Text where
    toRadicle = Rad.String

instance ToRadicle () where
    toRadicle _ = list $ []

instance ToRadicle a => ToRadicle (Set a) where
    toRadicle = list . map toRadicle . Set.toList

instance ToRadicle a => ToRadicle [a] where
    toRadicle = list . map toRadicle

--------------------------------------------------------------------------------

class FromRadicle a where
    fromRadicle :: Rad.Value -> a

instance FromRadicle Text where
    fromRadicle x = T.pack (show x)

-- | Return a Radicle value that evaluates to the given list of Radicle
-- values.
-- @
--      Rad.eval (list vals) = pure vals
-- @
list :: [Rad.Value] -> Rad.Value
list vals = Rad.List $ Rad.Primop (Rad.Ident "list"):vals

-- | Return a Radicle expression that applies @args@ to the function
-- named @fnName@.
fnApply :: Text -> [Rad.Value] -> Rad.Value
fnApply fnName args = Rad.List $ (Rad.Atom $ Rad.Ident fnName) : args
