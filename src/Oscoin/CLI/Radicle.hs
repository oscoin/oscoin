module Oscoin.CLI.Radicle where

import           Oscoin.Prelude

import qualified Radicle as Rad
import qualified Data.Set as Set
import           Data.Scientific (scientific)
import qualified Data.Text as T

class ToRadicle a where
    toRadicle :: a -> Rad.Value

instance ToRadicle Int where
    toRadicle x = Rad.Number (scientific (toInteger x) 0)

instance ToRadicle Text where
    toRadicle = Rad.String

instance ToRadicle () where
    toRadicle _ = Rad.List []

instance ToRadicle a => ToRadicle (Set a) where
    toRadicle = Rad.List . map toRadicle . Set.toList

instance ToRadicle a => ToRadicle [a] where
    toRadicle = Rad.List . map toRadicle

--------------------------------------------------------------------------------

class FromRadicle a where
    fromRadicle :: Rad.Value -> a

instance FromRadicle Text where
    fromRadicle x = T.pack (show x)

