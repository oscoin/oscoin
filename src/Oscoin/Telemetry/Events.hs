module Oscoin.Telemetry.Events
    ( NotableEvent(..)
    ) where

import           Oscoin.Prelude

-- | A \"NotableEvent\" is an event which is worthwhile observing, logging
-- and measuring.
data NotableEvent =
      BlockMinedEvent
    | BlockAppliedEvent
    | TxSentEvent
    | TxReceivedEvent
    deriving (Show, Eq, Ord)
