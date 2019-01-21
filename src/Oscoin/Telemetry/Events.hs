module Oscoin.Telemetry.Events
    ( NotableEvent(..)
    ) where

import qualified Oscoin.Consensus.Types as Consensus
import qualified Oscoin.Crypto.Blockchain.Eval as Eval
import qualified Oscoin.Crypto.Hash as Crypto

-- | A \"NotableEvent\" is an event which is worthwhile observing, logging
-- and measuring.
data NotableEvent where
    BlockReceivedEvent :: Crypto.Hash -> NotableEvent
    -- ^ Triggered every time a new block is received at the network level.
    BlockMinedEvent :: Crypto.Hash -> NotableEvent
    -- ^ Triggered every time a new block is mined.
    BlockAppliedEvent :: Crypto.Hash -> NotableEvent
    -- ^ Triggered every time a new block is (successfully) applied.
    BlockStaleEvent :: Crypto.Hash -> NotableEvent
    -- ^ Triggered every time a block we tried to apply was stale.
    BlockApplyErrorEvent :: Crypto.Hash -> NotableEvent
    -- ^ Triggered every time an action to apply a block resulted in an
    -- error.
    BlockOrphanEvent :: Crypto.Hash -> NotableEvent
    -- ^ Triggered every time a block is found to be an orphan.
    BlockValidationFailedEvent :: Crypto.Hash -> Consensus.ValidationError -> NotableEvent
    -- ^ Triggered every time a 'Block' fails to validate.
    BlockEvaluationFailedEvent :: Crypto.Hash -> Eval.EvalError -> NotableEvent
    -- ^ Triggered every time a 'Block' fails to evaluate.
    TxSentEvent :: forall tx. Crypto.Hashed tx -> NotableEvent
    -- ^ Triggered every time a transaction is successfully sent.
    TxSubmittedEvent :: forall tx. Crypto.Hashed tx -> NotableEvent
    -- ^ Triggered every time a transaction is successfully submitted via
    -- the HTTP API.
    TxReceivedEvent :: forall tx. Crypto.Hashed tx -> NotableEvent
    -- ^ Triggered every time a new tx is received at the network level.
    TxStaleEvent :: forall tx. Crypto.Hashed tx -> NotableEvent
    -- ^ Triggered every time we called 'applyTx' on a stale transaction.
    TxAppliedEvent :: forall tx. Crypto.Hashed tx -> NotableEvent
    -- ^ Triggered every time calling 'applyTx' resulted in an 'Applied' result.
    TxsAddedToMempoolEvent :: forall tx. Crypto.Hashable tx => [tx] -> NotableEvent
    -- ^ Triggered every time a new rad transaction was added to the mempool.
    TxsRemovedFromMempoolEvent :: forall tx. Crypto.Hashable tx => [tx] -> NotableEvent
    -- ^ Triggered every time a new rad transaction was removed from the mempool.
