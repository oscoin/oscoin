-- | Syncing nodes in oscoin
--
-- Based on the spec available at https://hackmd.io/2cPkrWTjTIWo2EmHZpPIQw
--

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Protocol.Sync
    (
    -- * Types
      SyncT(..)
    , SyncError(..)
    , Timeout(..)
    , ActivePeer
    , ActivePeers
    , SyncEvent(..)
    , SyncContext(..)
    , DataFetcher(..)
    , ProtocolRequest(..)
    , ProtocolRequestArgs
    , SProtocolRequest(..)
    , ProtocolResponse
    , Range(..)

    -- * Pure functions
    , range
    , isDone
    , height

    -- * Syncing functions
    , syncBlocks
    , sync

    -- * Recording events
    , recordEvent

    -- * Testing internals
    , syncUntil
    , withActivePeers
    , withActivePeer
    , getRemoteTip
    , getRemoteHeight
    ) where

import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Prelude
import           Prelude (last)

import           Oscoin.Crypto.Blockchain.Block
                 ( Beneficiary
                 , Block
                 , BlockHeader
                 , Height
                 , Sealed
                 , blockHash
                 , blockHeader
                 , blockHeight
                 )
import           Oscoin.P2P as P2P
import           Oscoin.Storage.Block.Abstract (BlockStoreReader, getTip)
import           Oscoin.Telemetry.Events (NotableEvent(NodeSyncEvent))
import qualified Oscoin.Telemetry.Events.Sync as Telemetry.Events
import           Oscoin.Telemetry.Trace as Telemetry
import           Oscoin.Time (Duration)

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.List ((\\))
import           Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Formatting.Buildable (Buildable)
import           GHC.Natural

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- | An active peer, i.e. one of the nodes we are connected to and to which we
-- can ask for information.
type ActivePeer c  = NodeInfo c

-- | The set of our active peers.
type ActivePeers c = HashSet (ActivePeer c)

-- | A 'SyncEvent' is generated every time we successfully download a block
-- or a block header from one of our peers.
data SyncEvent c tx s =
      SyncBlock (Block c tx (Sealed c s))
    | SyncBlockHeader (BlockHeader c s)

deriving instance ( Eq (Hash c)
                  , Eq tx
                  , Eq s
                  , Eq (Beneficiary c)
                  ) => Eq (SyncEvent c tx s)
deriving instance ( Ord (Hash c)
                  , Ord tx
                  , Ord s
                  , Ord (Beneficiary c)
                  ) => Ord (SyncEvent c tx s)
deriving instance ( Show (Hash c)
                  , Show tx
                  , Show s
                  , Show (Beneficiary c)
                  ) => Show (SyncEvent c tx s)

-- | A 'SyncError' is returned upon exceptional conditions when syncing; this
-- can happen (for example) if none of the active peers replies in a timely
-- fashion or if the requested information can't be retrieved.
data SyncError =
      RequestTimeout ProtocolRequest Addr Timeout
      -- ^ The given peer exceeded the request timeout when serving this
      -- 'SyncProtocolRequest'.
    | RequestNetworkError ProtocolRequest Addr String
    -- ^ NOTE (adn) Ideally this would carry an exception, but neither
    -- 'SomeException' nor 'HttpException' implements 'Eq'. We could theoretically
    -- implement 'Eq' manually for 'HttpException', but that wouldn't be a great
    -- idea anyway, not to mention we would expose the type of network communication
    -- to the callers. We cannot recover from a network failure anyway, so the
    -- 'String' here is merely used to inform upstream users of what went wrong.
    | NoActivePeers
      -- ^ There are no active peers to talk to.
    | NoRemoteTipFound
      -- ^ It was not possible to fetch a valid tip from our peers.
    | AllPeersSyncError [SyncError]
      -- ^ All the queried peers didn't respond in the allocated timeout.
    deriving (Show, Eq)

instance Exception SyncError

-- | Used by the 'timed' function to enforce a particular action doesn't
-- exceed the timeout.
data Timeout =
    MaxTimeoutExceeded (Expected Duration)
    -- ^ The operation exceeded the expected timeout, in nanoseconds.
    deriving (Show, Eq)

-- | A closed (inclusive) range [lo,hi].
data Range = Range
    { rangeStart :: Height
    , rangeEnd   :: Height
    } deriving Show

-- | An enumeration of all the requests a node can submit to its peers.
data ProtocolRequest =
      GetTip
    -- ^ Get the tip of the remote chain, i.e. the tip of the chain the
    -- active peers are following.
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
type family ProtocolRequestArgs (r :: ProtocolRequest) :: * where
    ProtocolRequestArgs 'GetTip          = ()
    ProtocolRequestArgs 'GetBlocks       = Range
    ProtocolRequestArgs 'GetBlockHeaders = Range

-- | A protocol response, indexed by the corresponding 'ProtocolRequest, so
-- that matching the wrong type will result in a type error.
type family ProtocolResponse c (m :: * -> *) tx s (r :: ProtocolRequest) :: * where
    ProtocolResponse c m tx s 'GetTip =
        Block c tx (Sealed c s)
    ProtocolResponse c m tx s 'GetBlocks =
        ConduitT () (Block c tx (Sealed c s)) m ()
    ProtocolResponse c m tx s 'GetBlockHeaders =
        ConduitT () (BlockHeader c (Sealed c s)) m ()

-- | A data fetcher is a wrapper around an action that given a (singleton)
-- 'SProtocolRequest' and a set of arguments, returns a 'ProtocolResponse'
-- while producting some side-effect in @m@.
newtype DataFetcher c tx s m =
    DataFetcher {
      fetch :: forall r. ActivePeer c
            -> SProtocolRequest r
            -> ProtocolRequestArgs r
            -> m (Either SyncError (ProtocolResponse c m tx s r))
                }

-- | An opaque reference to a record of functions which can be used to query
-- the network and the other parts of the system for information.
data SyncContext c tx s m = SyncContext
    { scNu               :: Natural
    -- ^ Used to determine the block tolerance to consider
    -- ourselves synced. It correspond to /nu/ from the spec, and is used
    -- in the calculation of 'isDone'.
    , scActivePeers       :: m (ActivePeers c)
    -- ^ An effectful action that can be used to get the set of active peers
    -- for this node.
    , scDataFetcher       :: DataFetcher c tx s m
    -- ^ A 'DataFetcher'.
    , scEventTracer       :: Tracer m
    -- ^ A tracer for interesting events that happens during syncing.
    , scConcurrently      :: forall a b t.  Traversable t => t a -> (a -> m b) -> m (t b)
    -- ^ An action that can be used to fetch some resources in parallel.
    -- 'IO'-based implementations will probably use 'forConcurrently' or some
    -- variation of it.
    , scLocalChainReader  :: BlockStoreReader c tx s m
    -- ^ A read-only handle to the local blockchain.
    , scEventHandlers :: [SyncEvent c tx s -> m ()]
    -- ^ A list of event handlers, represented as effectful actions from
    -- a 'SyncEvent' to 'm ()'.
    }

-- | A sync monad over @m@, that can throw 'SyncError's.
newtype SyncT c tx s m a =
    SyncT { runSyncT :: ExceptT SyncError (ReaderT (SyncContext c tx s m) m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError SyncError
             , MonadReader (SyncContext c tx s m)
             )

instance MonadTrans (SyncT c tx s) where
    lift = SyncT . lift . lift

instance MonadIO m => MonadIO (SyncT c tx s m) where
    liftIO = SyncT . liftIO

{------------------------------------------------------------------------------
  Convenience functions over SyncT
------------------------------------------------------------------------------}

withActivePeers
    :: Monad m
    => (ActivePeers c -> SyncT c tx s m a)
    -> SyncT c tx s m a
withActivePeers f = do
    ctx <- ask
    activePeers <- lift (scActivePeers ctx)
    when (HS.null activePeers) $ throwError NoActivePeers
    f activePeers

-- | Like 'withActivePeers', but it passes to the callback only a (single)
-- active peer. This might be useful in tests which it's not necessary to have
-- all the active peers available.
withActivePeer
    :: Monad m
    => (ActivePeer c -> SyncT c tx s m a)
    -> SyncT c tx s m a
withActivePeer f = do
    ctx <- ask
    activePeers <- lift (scActivePeers ctx)
    case HS.toList activePeers of
      []           -> throwError NoActivePeers
      activePeer:_ -> f activePeer

{------------------------------------------------------------------------------
  Pure functions & constants
------------------------------------------------------------------------------}

height :: Block c tx s -> Height
height = blockHeight . blockHeader

-- | The maximum number of blocks we can request to each of our peers in a
-- single request. It loosely correspond to the notion of "in flight blocks"
-- in Bitcoin's inventory exchange.
maxBlocksPerPeer :: Int
maxBlocksPerPeer = 50

-- | Returns 'True' if the node finished syncing all the blocks.
-- This is the 'isDone' state transition function from the spec.
isDone
    :: Natural
    -> Block c tx s
    -> Block c tx s
    -> Bool
isDone (fromIntegral -> nu) localTip remoteTip =
    height remoteTip - height localTip < nu

-- | Returns the range between two blocks.
range
    :: Block c tx s
    -> Block c tx s
    -> Maybe Range
range localTip remoteTip
  | outOfRange = Nothing
  | otherwise  = Just (Range start end)
  where
    start      = succ (height localTip)
    end        = height remoteTip
    outOfRange = end < start

{------------------------------------------------------------------------------
  Monadic operations
------------------------------------------------------------------------------}

-- | The 'remoteTip' operation from the spec. It tries to fetch the tips
-- concurrently, picking the tip returned by /most/ of the peers.
getRemoteTip
    :: forall c tx s m.
       ( Monad m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (Beneficiary c)
       ) => SyncT c tx s m (ProtocolResponse c m tx s 'GetTip)
getRemoteTip = withActivePeers $ \active -> do
    dataFetcher     <- scDataFetcher  <$> ask
    forConcurrently <- scConcurrently <$> ask
    results  <- lift $
        forConcurrently (HS.toList active) (\a -> fetch dataFetcher a SGetTip ())
    case partitionEithers results of
      (errs, [])           -> throwError (AllPeersSyncError errs)
      (_timeouts, allTips) ->
          -- Compare the result on the frequency, and pick the highest block
          -- in case of \"draws\".
          case sortBy comparingTips (M.toList . freqtable $ allTips) of
            []          -> throwError NoRemoteTipFound
            (t,_freq):_ -> pure t
  where
      -- Very simple frequency table implementation. Given a list of blocks,
      -- it builds a frequency map <block,frequency>.
      freqtable
          :: [Block c tx (Sealed c s)]
          -> M.Map (Block c tx (Sealed c s)) Int
      freqtable = foldl' (\acc h -> M.insertWith (+) h 1 acc) M.empty

      -- Compare tips, most frequent and highest first.
      comparingTips (b1,f1) (b2,f2) =
          case f2 `compare` f1 of
            EQ -> height b2 `compare` height b1
            x  -> x

-- | The 'bestHeight' operation from the spec.
getRemoteHeight
    :: ( Monad m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (Beneficiary c)
       )
    => SyncT c tx s m Height
getRemoteHeight = map height getRemoteTip

-- | Given a valid 'Range', syncs the blocks within that range.
syncBlocks
    :: forall c tx s m.
       ( Monad m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (Beneficiary c)
       , Buildable (Hash c)
       )
    => Range
    -> SyncT c tx s m ()
syncBlocks rng = do
    ctx <- ask
    let fetcher = scDataFetcher ctx
    withActivePeers $ \ activePeers ->

        lift $ runConduit $
               C.yieldMany (workDistribution activePeers)
            .| C.mapM (fetchBlocks fetcher >=> notifyUpstreamConsumers ctx)
            .| C.concat
            .| C.iterM (fetchMissing fetcher activePeers ctx)
            .| C.sinkNull

  where

    -- How many blocks a peer can have in-transit at any given time, to
    -- avoid network congestion.
    blocksPerPeer :: ActivePeers c -> Int
    blocksPerPeer peers =
        min maxBlocksPerPeer
            (ceiling @Double @Int (fromIntegral (rangeEnd rng - rangeStart rng) / fromIntegral (length peers)))

    -- Distribute work across peers, which is equally split, where
    -- each peer can be given /at most/ 'maxBlocksPerPeer' at the time,
    -- in order to limit the amount of blocks in transit each time.
    workDistribution :: ActivePeers c -> [(ActivePeer c, [Height])]
    workDistribution peers =
        zip (cycle (HS.toList peers))
            (chunksOf (blocksPerPeer peers) [rangeStart rng .. rangeEnd rng])

    -- Fetch some blocks from an active peer, and returns any fetched block
    -- plus any missing block, identified by the height.
    fetchBlocks
        :: DataFetcher c tx s m
        -> (ActivePeer c, [Height])
        -> m (Set (Block c tx (Sealed c s)), [Height])
    fetchBlocks fetcher (activePeer, !heightRange) =
      case Range <$> head heightRange <*> pure (last heightRange) of
          Nothing -> pure (mempty, mempty)
          Just subRange -> do
              res <- fetch fetcher activePeer SGetBlocks subRange
              case res of
                Left _timeout ->
                    pure (mempty, heightRange) -- TODO(adn) Telemetry?
                Right blks    ->
                    runConduit $ blks
                              .| C.foldl (\(requested, missing) blk ->
                                  ( Set.insert blk requested
                                  , missing \\ [height blk])
                                  ) (mempty, heightRange)

    -- Notifies the upstream consumers about the downloaded blocks, and
    -- forward the missing blocks downstream.
    notifyUpstreamConsumers
        :: SyncContext c tx s m
        -> (Set (Block c tx (Sealed c s)), [Height])
        -> m [Height]
    notifyUpstreamConsumers ctx (requested, missing) = do
        forM_ requested $ \b ->
            forM_ (scEventHandlers ctx) $ \dispatch ->
                dispatch (SyncBlock b)
        recordEvent' @c (scEventTracer ctx) $
            Telemetry.Events.NodeSyncFetched (length requested)
        recordEvent' @c (scEventTracer ctx) $
            Telemetry.Events.NodeSyncMissing (length missing)
        pure missing

    -- Tries to fetch a missing block (as identified by its 'Height') from
    -- any of the active peers.
    fetchMissing
        :: DataFetcher c tx s m
        -> ActivePeers c
        -> SyncContext c tx s m
        -> Height
        -> m ()
    fetchMissing fetcher peers ctx missing = do
       -- Tries to download each individual block from each and every
       -- peer. We don't fail in case the block cannot be downloaded,
       -- as we hope that eventually the peers will be able to catch up.
       mbBlock <- foldM (\mbBlock peer ->
                    case mbBlock of
                      Just b  -> pure (Just b)
                      Nothing -> do
                          res <- fetch fetcher peer SGetBlocks (Range missing missing)
                          case res of
                            Left _err -> pure Nothing
                            Right c   -> runConduit (c .| C.head)
                        ) Nothing (HS.toList peers)
       case mbBlock of
         Nothing -> pure () -- TODO(adn) telemetry?
         Just b -> forM_ (scEventHandlers ctx) $ \dispatch ->
                       dispatch (SyncBlock b)

-- | Calls 'isDone' internally, and try syncing blocks if it returns 'False'.
syncUntil
    :: forall c tx s m.
       ( Monad m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (Beneficiary c)
       , Buildable (Hash c)
       )
    => (SyncContext c tx s m -> Block c tx (Sealed c s) -> Block c tx (Sealed c s) -> Bool)
    -> SyncT c tx s m ()
syncUntil finished = do
    ctx <- ask
    remoteTip <- getRemoteTip
    localTip  <- lift (getTip (scLocalChainReader ctx))
    unlessDone ctx localTip remoteTip $
        for_ (range localTip remoteTip) syncBlocks

  where
    unlessDone
        :: SyncContext c tx s m
        -> Block c tx (Sealed c s)
        -> Block c tx (Sealed c s)
        -> SyncT c tx s m ()
        -> SyncT c tx s m ()
    unlessDone ctx localTip remoteTip action =
        if finished ctx localTip remoteTip
           then recordEvent $
                Telemetry.Events.NodeSyncFinished (blockHash localTip, height localTip)
        else do
            recordEvent $
                Telemetry.Events.NodeSyncStarted (blockHash localTip, height localTip)
                                                 (blockHash remoteTip, height remoteTip)

            action

            newLocalTip <- lift (getTip (scLocalChainReader ctx))
            recordEvent $
                Telemetry.Events.NodeSyncFinished (blockHash newLocalTip, height newLocalTip)

sync
    :: ( Monad m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (Beneficiary c)
       , Buildable (Hash c)
       )
    => SyncT c tx s m ()
sync = syncUntil (\ctx lcl rmt -> isDone (scNu ctx) lcl rmt)


{------------------------------------------------------------------------------
  Record telemetry events
------------------------------------------------------------------------------}

recordEvent
    :: ( Monad m
       , Buildable (Hash c)
       )
    => Telemetry.Events.NodeSyncEvent c
    -> SyncT c tx s m ()
recordEvent evt = do
    ctx <- ask
    lift $ recordEvent' (scEventTracer ctx) evt

recordEvent'
    :: Buildable (Hash c)
    => Tracer m
    -> Telemetry.Events.NodeSyncEvent c
    -> m ()
recordEvent' eventTracer evt =
    eventTracer $ Telemetry.traced (NodeSyncEvent evt) ()
