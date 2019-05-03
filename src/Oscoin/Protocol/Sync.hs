{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Protocol.Sync
    ( syncNode

    -- * Types
    , Sync(..)
    , SyncError(..)
    , ActivePeer
    , ActivePeers
    , NetworkLayer(..)
    , DataFetcher(..)
    , SProtocolRequest(..)
    , ProtocolResponse(..)
    , Range(..)

    -- * Testing internals
    , withActivePeers
    ) where

import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Prelude

import           Oscoin.Consensus.Nakamoto (PoW)
import           Oscoin.Crypto.Blockchain.Block
                 ( Block
                 , BlockHash
                 , BlockHeader
                 , Height
                 , Sealed
                 , blockHeader
                 , blockHeight
                 )
import           Oscoin.Crypto.Hash (HasHashing)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx
import           Oscoin.P2P as P2P
import           Oscoin.Telemetry.Trace
import           Oscoin.Time.Chrono

import           Codec.Serialise as CBOR
import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Network.Gossip.HyParView as Gossip
import           Network.Gossip.IO.Peer (Peer(..))
import qualified Network.HTTP.Client as HTTP

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type ActivePeer c  = NodeInfo c
type ActivePeers c = HashSet (ActivePeer c)

data SyncError c =
      RequestTimeout ProtocolRequest (NodeInfo c) Int
      -- ^ The given peer exceeded the request timeout when serving this
      -- 'SyncProtocolRequest'.
    | NoActivePeers
      -- ^ There are not active peers to talk to.

deriving instance (Show (Crypto.PublicKey c)) => Show (SyncError c)
deriving instance (Eq (Crypto.PublicKey c)) => Eq (SyncError c)

-- | A closed range [lo,hi].
data Range = Range
    { start :: Height
    , end   :: Height
    }

data ProtocolRequest =
      GetTip
    -- ^ Get the tip of the best chain.
    | GetBlocks
    -- ^ Get some blocks.
    | GetBlockHeaders
    -- ^ Get some block headers.
    deriving (Show, Eq)

-- | The singleton type for 'ProtocolRequest', capable of carrying some
-- witness (in this case, 'ProtocolRequest').
data SProtocolRequest r where
  SGetTip :: SProtocolRequest 'GetTip
  SGetBlocks :: SProtocolRequest 'GetBlocks
  SGetBlockHeaders :: SProtocolRequest 'GetBlockHeaders

-- | A closed type family mapping each 'ProtocolRequest' to its list of
-- arguments, so that we can use the 'ProtocolRequest' (where all constructors
-- have kind /*/) as a constraint.
type family ProtocolRequestArgs (c :: ProtocolRequest) :: * where
    ProtocolRequestArgs 'GetTip          = ()
    ProtocolRequestArgs 'GetBlocks       = Range
    ProtocolRequestArgs 'GetBlockHeaders = Range

-- | A protocol response, indexed by the corresponding 'ProtocolRequest, so
-- that matching the wrong type will result in a type error.
data ProtocolResponse (r :: ProtocolRequest) where
    GetTipResp :: forall c tx s. Serialise (BlockHash c)
               => Block c tx (Sealed c s)
               -> ProtocolResponse 'GetTip
    GetBlocksResp :: forall c tx s. (NewestFirst [] (Block c tx (Sealed c s)))
                  -> ProtocolResponse 'GetBlocks
    GetBlockHeadersResp :: forall c s. (NewestFirst [] (BlockHeader c (Sealed c s)))
                  -> ProtocolResponse 'GetBlockHeaders

-- | A data fetcher is a wrapper around an action that given a (singleton)
-- 'SProtocolRequest' and a set of arguments, returns a 'ProtocolResponse'
-- while producting some side-effect in @m@.
newtype DataFetcher c m =
    DataFetcher {
      fetch :: forall r. ActivePeer c
            -> SProtocolRequest r
            -> ProtocolRequestArgs r
            -> m (ProtocolResponse r)
                }

-- | An opaque reference to a record of functions which can be used to query
-- the network for information.
data NetworkLayer c m = NetworkLayer
    { nlActivePeers :: m (ActivePeers c)
    , nlDataFetcher :: DataFetcher c m
    , nlEventTracer :: Tracer m
    }

newtype Sync c m a =
    Sync { runSync :: ExceptT (SyncError c) (ReaderT (NetworkLayer c m) m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError (SyncError c)
             , MonadReader (NetworkLayer c m)
             )

instance MonadTrans (Sync c) where
    lift = Sync . lift . lift

{------------------------------------------------------------------------------
  Convenience functions over Sync
------------------------------------------------------------------------------}

withActivePeers :: Monad m => (ActivePeers c -> Sync c m a) -> Sync c m a
withActivePeers f = do
    net <- ask
    activePeers <- lift (nlActivePeers net)
    when (HS.null activePeers) $ throwError NoActivePeers
    f activePeers

{------------------------------------------------------------------------------
  Scary IO implementation
------------------------------------------------------------------------------}

newDataFetcherIO
    :: forall c.
    ( Serialise (BlockHash c)
    , Serialise (Crypto.PublicKey c)
    , Serialise (Crypto.Signature c)
    , HasHashing c
    )
    => HTTP.Manager
    -> DataFetcher c IO
newDataFetcherIO httpManager = DataFetcher $ \peer -> \case
    SGetTip -> \() -> do
        rq0 <- HTTP.parseUrlThrow "/blockchain/tip"
        let rq = rq0 { HTTP.port = 8477 -- FIXME(adn) broadcast & handshake.
                     }

       -- Trying to issue a wrong response type will trigger a compilation
       -- error similar to:
       -- • Couldn't match type ‘'GetBlockHeaders’ with ‘'GetTip’
       -- Expected type: IO (ProtocolResponse r)
       -- Actual type: IO (ProtocolResponse 'GetBlockHeaders)

        HTTP.withResponse rq httpManager $ \res -> do
            cborBlob <- HTTP.brRead (HTTP.responseBody res)
            pure $ GetTipResp @c @(Tx c) @PoW (CBOR.deserialise . toS $ cborBlob)

    SGetBlocks -> \Range{..} -> notImplemented
    SGetBlockHeaders -> \Range{..} -> notImplemented

-- | Constructs a new 'Tracer' over @IO@.
newEventTracerIO :: Probe IO -> Tracer IO
newEventTracerIO = probed

-- | Constructs a new 'NetworkLayer' over @IO@.
newNetworkLayerIO
    :: ( Eq (Crypto.PublicKey c)
       , Hashable (Crypto.PublicKey c)
       , Serialise (BlockHash c)
       , Serialise (Crypto.PublicKey c)
       , Serialise (Crypto.Signature c)
       , HasHashing c
       )
    => Probe IO
    -> GossipT c IO (NetworkLayer c IO)
newNetworkLayerIO probe = do
    env <- ask
    mgr <- liftIO (HTTP.newManager HTTP.defaultManagerSettings)
    pure $ NetworkLayer
        { nlActivePeers = HS.map peerNodeId . Gossip.active <$> P2P.getPeers' env
        , nlDataFetcher = newDataFetcherIO mgr
        , nlEventTracer = newEventTracerIO probe
        }

{------------------------------------------------------------------------------
  Monadic operations
------------------------------------------------------------------------------}

bestTip :: Sync c m (Block c tx s)
bestTip = notImplemented

{------------------------------------------------------------------------------
  Pure functions
------------------------------------------------------------------------------}

height :: Block c tx s -> Height
height = blockHeight . blockHeader

-- | The mutable chain suffix, from the spec.
type Nu = Integer

-- | Returns 'True' if the difference between the height of the best tip and
-- the tip of the node falls within the \"mutable range\" of the blockchain.
isMutable :: Nu -> Block c tx s -> Block c tx s -> Bool
isMutable nu myTip bestRemoteTip =
    height bestRemoteTip - height myTip <= nu

-- | Returns 'True' if the node finished syncing all the blocks.
isDone
    :: ( Eq tx
       , Eq s
       , Eq (Hash c)
       )
    => Block c tx s -> Block c tx s -> Bool
isDone myTip bestRemoteTip = bestRemoteTip == myTip

{------------------------------------------------------------------------------
  Monad-agnostic algorithm
------------------------------------------------------------------------------}

-- | The syncing algorithm proper.
syncNode :: NetworkLayer c m -> m ()
syncNode = notImplemented
