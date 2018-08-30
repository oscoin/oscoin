module Oscoin.CLI.User where

import           Oscoin.CLI.Radicle
import           Oscoin.Prelude

newtype User = User Text
    deriving (Show, IsString, Eq, Ord, ToRadicle)
