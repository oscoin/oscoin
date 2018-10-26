module Network.Gossip.Test.Assert (allEqual) where

import           Prelude

import           Data.List.NonEmpty (NonEmpty(..))

allEqual :: Eq a => NonEmpty a -> Bool
allEqual (x :| xs) = all (== x) xs
