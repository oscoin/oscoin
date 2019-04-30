module Oscoin.Telemetry.Events
    ( NotableEvent(..)
    ) where

import           Oscoin.Prelude

import qualified Oscoin.Consensus.Types as Consensus
import           Oscoin.Crypto.Blockchain.Block.Difficulty (Difficulty)
import qualified Oscoin.Crypto.Blockchain.Eval as Eval
import           Oscoin.Crypto.Hash (HasHashing, Hash, Hashable, Hashed)
import           Oscoin.Crypto.PubKey (PublicKey)
import qualified Oscoin.P2P.Disco as P2P.Disco
import qualified Oscoin.P2P.Types as P2P
import           Oscoin.Time (Duration)

import qualified Network.Gossip.IO.Trace as Gossip (Traceable)

import           Formatting.Buildable (Buildable)
import           Network.HTTP.Types as HTTP
import           Network.Wai as HTTP

-- | A \"NotableEvent\" is an event which is worthwhile observing, logging
-- and measuring.
data NotableEvent where
    -- | Triggered every time a new block is received at the network level.
    BlockReceivedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                       => Hash c -> NotableEvent
    -- | Triggered every time a new block is mined.
    BlockMinedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                    => Hash c -> NotableEvent
    -- | Triggered when broadcasting a newly mined block fails.
    BlockBroadcastFailedEvent
        :: forall c. (Buildable (Hash c), HasHashing c)
        => Hash c
        -> SomeException
        -> NotableEvent
    -- | Triggered when broadcasting a newly mined block succeeded.
    BlockBroadcastEvent
        :: forall c. (Buildable (Hash c), HasHashing c)
        => Hash c
        -> NotableEvent
    -- | Triggered every time a new block is (successfully) applied.
    BlockAppliedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                      => Hash c -> NotableEvent
    -- | Triggered every time a block we tried to apply was stale.
    BlockStaleEvent :: forall c. (Buildable (Hash c), HasHashing c)
                    => Hash c -> NotableEvent
    -- | Triggered every time an action to apply a block resulted in an
    -- error.
    BlockApplyErrorEvent :: forall c. (Buildable (Hash c), HasHashing c)
                         => Hash c -> NotableEvent
    -- | Triggered every time a block is found to be an orphan.
    BlockOrphanEvent :: forall c. (Buildable (Hash c), HasHashing c)
                     => Hash c -> NotableEvent
    -- | Triggered every time a 'Block' fails to validate.
    BlockValidationFailedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                               => Hash c
                               -> Consensus.ValidationError c
                               -> NotableEvent
    -- | Triggered every time a 'Block' fails to evaluate.
    BlockEvaluationFailedEvent :: forall c. (Buildable (Hash c), HasHashing c)
                               => Hash c
                               -> Eval.EvalError
                               -> NotableEvent
    -- | Triggered when the 'Difficulty' is adjusted. The first argument is
    -- the new difficulty, the second the (now) previous one.
    DifficultyAdjustedEvent :: Difficulty
                            -> Difficulty
                            -> NotableEvent
    -- | Triggered every time a transaction is successfully sent.
    TxSentEvent :: forall c tx. Buildable (Hash c)
                => Hashed c tx
                -> NotableEvent
    -- | Triggered every time a transaction is successfully submitted via
    -- the HTTP API.
    TxSubmittedEvent :: forall c tx. Buildable (Hash c)
                     => Hashed c tx
                     -> NotableEvent
    -- | Triggered every time a new tx is received at the network level.
    TxReceivedEvent :: forall c tx. Buildable (Hash c)
                    => Hashed c tx
                    -> NotableEvent
    -- | Triggered every time we called 'applyTx' on a stale transaction.
    TxStaleEvent :: forall c tx. Buildable (Hash c)
                 => Hashed c tx
                 -> NotableEvent
    -- | Triggered every time calling 'applyTx' resulted in an 'Applied' result.
    TxAppliedEvent :: forall c tx. Buildable (Hash c)
                   => Hashed c tx
                   -> NotableEvent
    -- | Triggered every time a new rad transaction was added to the mempool.
    TxsAddedToMempoolEvent :: forall c. Buildable (Hash c)
                           => [Hash c]
                           -> NotableEvent
    -- | Triggered every time a new rad transaction was removed from the mempool.
    TxsRemovedFromMempoolEvent :: forall c. Buildable (Hash c)
                               => [Hash c]
                               -> NotableEvent
    -- | Triggered every time the P2P layer returns a 'ConversionError'.
    Peer2PeerErrorEvent :: P2P.ConversionError -> NotableEvent
    -- | Triggered every time a new HTTP request is issued to the node's API.
    HttpApiRequest :: HTTP.Request -> HTTP.Status -> Duration -> NotableEvent
    -- | Events emitted by the @gossip@ library
    GossipEvent :: forall c. (Hashable c (PublicKey c), Buildable (Hash c))
                => Gossip.Traceable (P2P.NodeId c)
                -> NotableEvent
    -- | Events emitted during the p2p handshake phase
    HandshakeEvent :: forall c. (Hashable c (PublicKey c), Buildable (Hash c))
                   => P2P.HandshakeEvent (P2P.NodeId c)
                   -> NotableEvent
    -- | Peer discovery discovery events
    DiscoEvent :: P2P.Disco.DiscoEvent -> NotableEvent
