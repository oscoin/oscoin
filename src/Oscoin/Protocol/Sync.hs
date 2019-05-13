{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Protocol.Sync
    ( syncNode

    -- * Types
    , Sync(..)
    , SyncError(..)
    , Timeout(..)
    , ActivePeer
    , ActivePeers
    , SyncEvent(..)
    , SyncContext(..)
    , DataFetcher(..)
    , ProtocolRequest(..)
    , SProtocolRequest(..)
    , ProtocolResponse
    , Range(..)

    -- * Pure functions
    , isMutable
    , isDone
    , immutableRange

    -- * Testing internals
    , withActivePeers
    , bestTip
    , bestHeight
    ) where

import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Prelude

import           Oscoin.Consensus.Nakamoto (PoW)

import           Oscoin.Crypto.Blockchain.Block
                 (Block, BlockHeader, Height, Sealed, blockHeader, blockHeight)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Data.Tx
import           Oscoin.P2P as P2P
import           Oscoin.Storage.Block.Abstract (BlockStoreReader)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Telemetry.Trace
import           Oscoin.Time (Duration)
import           Oscoin.Time.Chrono

import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           GHC.Natural

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type ActivePeer c  = NodeInfo c
type ActivePeers c = HashSet (ActivePeer c)

data SyncEvent c tx s=
      SyncBlock (Block c tx (Sealed c s))
    | SyncBlockHeader (BlockHeader c s)

data SyncError c =
      RequestTimeout ProtocolRequest (NodeInfo c) Timeout
      -- ^ The given peer exceeded the request timeout when serving this
      -- 'SyncProtocolRequest'.
    | NoActivePeers
      -- ^ There are not active peers to talk to.
    | NoBestTipFound
      -- ^ It was not possible to fetch a valid tip from our peers.
    | AllPeersTimeoutError
      -- ^ All the queried peers didn't respond in the allocated timeout.

data Timeout =
    MaxTimeoutExceeded (Expected Duration)
    -- ^ The operation exceeded the expected timeout, in nanoseconds.
    deriving (Show, Eq)

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
type family ProtocolResponse c tx s (r :: ProtocolRequest) :: *

type instance ProtocolResponse c (Tx c) PoW 'GetTip =
        Block c (Tx c) (Sealed c PoW)
type instance ProtocolResponse c (Tx c) PoW 'GetBlocks =
        OldestFirst [] (Block c (Tx c) (Sealed c PoW))
type instance ProtocolResponse c tx PoW 'GetBlockHeaders =
        OldestFirst [] (BlockHeader c (Sealed c PoW))

-- | A data fetcher is a wrapper around an action that given a (singleton)
-- 'SProtocolRequest' and a set of arguments, returns a 'ProtocolResponse'
-- while producting some side-effect in @m@.
newtype DataFetcher c tx s m =
    DataFetcher {
      fetch :: forall r. ActivePeer c
            -> SProtocolRequest r
            -> ProtocolRequestArgs r
            -> m (Either Timeout (ProtocolResponse c tx s r))
                }

-- | An opaque reference to a record of functions which can be used to query
-- the network and the other parts of the system for information.
data SyncContext c tx s m = SyncContext
    { scNu               :: Nu
    -- ^ The number of maximum allowed rollbacks. It corresponds to /nu/ from
    -- the spec and it's known as the 'mutableChainSuffix' in the rest of the
    -- codebase.
    , scActivePeers      :: m (ActivePeers c)
    , scDataFetcher      :: DataFetcher c tx s m
    , scEventTracer      :: Tracer m
    , scConcurrently     :: forall a b t.  Traversable t => t a -> (a -> m b) -> m (t b)
    , scLocalChainReader :: BlockStoreReader c tx s m
    }

-- | A sync monad over @m@, that can throw 'SyncError's.
newtype Sync c tx s m a =
    Sync { runSync :: ExceptT (SyncError c) (ReaderT (SyncContext c tx s m) m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError (SyncError c)
             , MonadReader (SyncContext c tx s m)
             )

instance MonadTrans (Sync c tx s) where
    lift = Sync . lift . lift

{------------------------------------------------------------------------------
  Convenience functions over Sync
------------------------------------------------------------------------------}

withActivePeers
    :: Monad m
    => (ActivePeers c -> Sync c tx s m a)
    -> Sync c tx s m a
withActivePeers f = do
    ctx <- ask
    activePeers <- lift (scActivePeers ctx)
    when (HS.null activePeers) $ throwError NoActivePeers
    f activePeers

{------------------------------------------------------------------------------
  Monadic operations
------------------------------------------------------------------------------}

-- The 'bestTip' operation from the spec. It tries to fetch the tips
-- concurrently, and picks the highest one.
-- TODO(adn) /Optimisation/: Build a frequency map and pick the one
-- returned by most peers. In case of draws, pick the highest one.
bestTip
    :: ( ProtocolResponse c tx s 'GetTip ~ Block c tx (Sealed c s)
       , Monad m
       ) => Sync c tx s m (ProtocolResponse c tx s 'GetTip)
bestTip = withActivePeers $ \active -> do
    dataFetcher     <- scDataFetcher  <$> ask
    forConcurrently <- scConcurrently <$> ask
    results  <- lift $
        forConcurrently (HS.toList active) (\a -> fetch dataFetcher a SGetTip ())
    case partitionEithers results of
      (_t:_, [])           -> throwError AllPeersTimeoutError
      ([], [])             -> throwError NoBestTipFound
      (_timeouts, allTips) -> pure $ maximumBy highest allTips
  where
      highest b1 b2 = height b1 `compare` height b2

bestHeight
    :: ( ProtocolResponse c tx s 'GetTip ~ Block c tx (Sealed c s)
       , Monad m
       )
    => Sync c tx s m Height
bestHeight = map height bestTip


syncImmutable
    :: ( ProtocolResponse c tx s 'GetTip ~ Block c tx (Sealed c s)
       , ProtocolResponse c tx s 'GetBlocks ~ OldestFirst [] (Block c tx (Sealed c s))
       , Hashable (Block c tx (Sealed c s))
       , Monad m
       , Eq tx
       , Eq s
       , Eq (Hash c)
       )
    => Sync c tx s m ()
syncImmutable = do
    ctx <- ask
    let nu      = scNu ctx
    let fetcher = scDataFetcher ctx

    bestRemoteTip <- bestTip
    localTip      <- lift (BlockStore.getTip (scLocalChainReader ctx))
    unless (isDone localTip bestRemoteTip) $ do
        case immutableRange nu localTip bestRemoteTip of
          Nothing | isMutable nu localTip bestRemoteTip ->
              syncMutable
          Nothing  -> pure ()
          Just rng -> withActivePeers $ \ activePeers -> do
              -- FIXME(adn) Extremely naive and wrong implementation
              -- for now, we basically request the /full range/ to everybody,
              -- filtering out dupes, which is not feasible for production and
              -- is terribly wasteful, obviously. A better strategy might be
              -- to split the work equally into multiple (capped) ranges
              -- (say, 500 blocks at the time) and then try to fetch each of
              -- those from all the peers.
              requestedBlocks <- lift $
                  foldM (\blocks activePeer -> do
                           res <- fetch fetcher activePeer SGetBlocks rng
                           case res of
                             Left _timeout -> pure blocks -- TODO(adn) Telemetry
                             Right blks    -> pure $ foldl' (flip HS.insert) blocks (toOldestFirst blks)
                        )
                        HS.empty
                        activePeers
              forM (HS.toList requestedBlocks) $ \_ -> pure ()

              -- Chase the best tip, which might have moved in the meantime
              syncImmutable

syncMutable
    :: ( ProtocolResponse c tx s 'GetTip ~ Block c tx (Sealed c s)
       , Monad m
       )
    => Sync c tx s m ()
syncMutable = notImplemented

{------------------------------------------------------------------------------
  Pure functions
------------------------------------------------------------------------------}

height :: Block c tx s -> Height
height = blockHeight . blockHeader

-- | The mutable chain suffix, from the spec.
type Nu = Natural

-- | Returns 'True' if the difference between the height of the best tip and
-- the tip of the node falls within the \"mutable range\" of the blockchain.
isMutable :: Nu -> Block c tx s -> Block c tx s -> Bool
isMutable nu myTip bestRemoteTip =
    height bestRemoteTip - height myTip <= toInteger nu

-- | Returns 'True' if the node finished syncing all the blocks.
isDone
    :: ( Eq tx
       , Eq s
       , Eq (Hash c)
       )
    => Block c tx s -> Block c tx s -> Bool
isDone myTip bestRemoteTip = bestRemoteTip == myTip

-- | Returns the immutable range of blocks that can be safely fetched without
-- worrying about rollbacks.
immutableRange
    :: Nu
    -> Block c tx s
    -> Block c tx s
    -> Maybe Range
immutableRange nu localTip remoteBestTip
  | outOfRange = Nothing
  | otherwise  = Just (Range start end)
  where
    start      = succ (height localTip)
    end        = height remoteBestTip - toInteger nu
    outOfRange = end < start

{------------------------------------------------------------------------------
  Monad-agnostic algorithm
------------------------------------------------------------------------------}

-- | The syncing algorithm proper.
syncNode :: SyncContext c tx s m -> m ()
syncNode = notImplemented
