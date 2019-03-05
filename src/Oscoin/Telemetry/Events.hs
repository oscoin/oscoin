module Oscoin.Telemetry.Events
    ( NotableEvent(..)
    ) where

import qualified Oscoin.Consensus.Types as Consensus
import qualified Oscoin.Crypto.Blockchain.Eval as Eval
import           Oscoin.Crypto.Hash (HasHashing, Hash, Hashable, Hashed)
import           Oscoin.Crypto.PubKey (PK)
import qualified Oscoin.P2P.Types as P2P
import           Oscoin.Time (Duration)

import qualified Network.Gossip.IO.Trace as Gossip (Traceable)

import           Formatting.Buildable (Buildable)
import           Network.HTTP.Types as HTTP
import           Network.Wai as HTTP

-- | A \"NotableEvent\" is an event which is worthwhile observing, logging
-- and measuring.
data NotableEvent where
    BlockReceivedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                       => Hash c -> NotableEvent
    -- ^ Triggered every time a new block is received at the network level.
    BlockMinedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                    => Hash c -> NotableEvent
    -- ^ Triggered every time a new block is mined.
    BlockAppliedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                      => Hash c -> NotableEvent
    -- ^ Triggered every time a new block is (successfully) applied.
    BlockStaleEvent :: forall c. (Buildable (Hash c), HasHashing c)
                    => Hash c -> NotableEvent
    -- ^ Triggered every time a block we tried to apply was stale.
    BlockApplyErrorEvent :: forall c. (Buildable (Hash c), HasHashing c)
                         => Hash c -> NotableEvent
    -- ^ Triggered every time an action to apply a block resulted in an
    -- error.
    BlockOrphanEvent :: forall c. (Buildable (Hash c), HasHashing c)
                     => Hash c -> NotableEvent
    -- ^ Triggered every time a block is found to be an orphan.
    BlockValidationFailedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                               => Hash c
                               -> Consensus.ValidationError c
                               -> NotableEvent
    -- ^ Triggered every time a 'Block' fails to validate.
    BlockEvaluationFailedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                               => Hash c
                               -> Eval.EvalError
                               -> NotableEvent
    -- ^ Triggered every time a 'Block' fails to evaluate.
    TxSentEvent :: forall c tx. Buildable (Hash c)
                => Hashed c tx
                -> NotableEvent
    -- ^ Triggered every time a transaction is successfully sent.
    TxSubmittedEvent :: forall c tx. Buildable (Hash c)
                     => Hashed c tx
                     -> NotableEvent
    -- ^ Triggered every time a transaction is successfully submitted via
    -- the HTTP API.
    TxReceivedEvent :: forall c tx. Buildable (Hash c)
                    => Hashed c tx
                    -> NotableEvent
    -- ^ Triggered every time a new tx is received at the network level.
    TxStaleEvent :: forall c tx. Buildable (Hash c)
                 => Hashed c tx
                 -> NotableEvent
    -- ^ Triggered every time we called 'applyTx' on a stale transaction.
    TxAppliedEvent :: forall c tx. Buildable (Hash c)
                   => Hashed c tx
                   -> NotableEvent
    -- ^ Triggered every time calling 'applyTx' resulted in an 'Applied' result.
    TxsAddedToMempoolEvent :: forall c. Buildable (Hash c)
                           => [Hash c]
                           -> NotableEvent
    -- ^ Triggered every time a new rad transaction was added to the mempool.
    TxsRemovedFromMempoolEvent :: forall c. Buildable (Hash c)
                               => [Hash c]
                               -> NotableEvent
    -- ^ Triggered every time a new rad transaction was removed from the mempool.
    Peer2PeerErrorEvent :: P2P.ConversionError -> NotableEvent
    -- ^ Triggered every time the P2P layer returns a 'ConversionError'.
    HttpApiRequest :: HTTP.Request -> HTTP.Status -> Duration -> NotableEvent
    -- ^ Triggered every time a new HTTP request is issued to the node's API.
    GossipEvent :: forall c. (Hashable c (PK c), Buildable (Hash c))
                => Gossip.Traceable (P2P.NodeId c)
                -> NotableEvent
    -- ^ Events emitted by the @gossip@ library
    HandshakeEvent :: forall c. (Hashable c (PK c), Buildable (Hash c))
                   => P2P.HandshakeEvent (P2P.NodeId c)
                   -> NotableEvent
    -- ^ Events emitted during the p2p handshake phase
