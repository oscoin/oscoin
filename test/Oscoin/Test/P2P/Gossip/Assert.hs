module Oscoin.Test.P2P.Gossip.Assert
    ( allEqual
    ) where

import           Oscoin.Prelude

allEqual :: Eq a => NonEmpty a -> Bool
allEqual (x :| xs) = all (== x) xs
