module Oscoin.Telemetry.Events
    ( NotableEvent(..)
    ) where

import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Prelude

-- | A \"NotableEvent\" is an event which is worthwhile observing, logging
-- and measuring.
data NotableEvent =
      BlockMinedEvent Crypto.Hash
    | BlockAppliedEvent
    | TxSentEvent
    | TxReceivedEvent
    deriving (Show, Eq, Ord)
