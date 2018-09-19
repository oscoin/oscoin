module Oscoin.CLI.Changeset where

import           Oscoin.CLI.Radicle
import           Oscoin.Prelude

type Commit = ()

newtype Changeset = Changeset [Commit]
    deriving (Show, ToRadicle, Semigroup, Monoid)
