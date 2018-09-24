module Oscoin.CLI.User where

import           Oscoin.Prelude

import           Radicle.Conversion

newtype User = User Text
    deriving (Show, IsString, Eq, Ord, ToRadicle, FromRadicle)
