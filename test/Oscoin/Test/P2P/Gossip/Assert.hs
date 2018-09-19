module Oscoin.Test.P2P.Gossip.Assert
    ( allEqual
    ) where

import           Oscoin.Prelude

allEqual :: Eq a => [a] -> Bool
allEqual []     = error $ "allEqual: vacuously true (empty list)"
allEqual (x:xs) = all (== x) xs
