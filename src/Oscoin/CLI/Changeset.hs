module Oscoin.CLI.Changeset where

import           Oscoin.Prelude
import           Oscoin.CLI.Radicle

type Commit = ()

newtype Changeset = Changeset [Commit]
    deriving (Show, ToRadicle)
