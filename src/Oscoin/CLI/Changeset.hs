module Oscoin.CLI.Changeset where

import           Oscoin.Prelude

import           Radicle.Conversion

type Commit = ()

newtype Changeset = Changeset [Commit]
    deriving (Show, Semigroup, Monoid, ToRadicle, FromRadicle)
