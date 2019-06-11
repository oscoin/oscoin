-- | Syncing nodes in oscoin
--
-- Based on the spec available at https://hackmd.io/2cPkrWTjTIWo2EmHZpPIQw
--

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Protocol.Sync
    (
    -- * Types
      SyncT(..)
    , SyncError(..)
    , Timeout(..)
    , LocalTip
    , RemoteTip
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
    , commonChainHeight
    , syncBlocks
    , sync

    -- * Recording events
    , recordEvent

    -- * Testing internals
    , RetryPolicy(..)
    , commonChainHeightWith
    , syncUntil
    , withActivePeers
    , withActivePeer
    , getRemoteTip
    , getRemoteHeight
    , linearBacktrackPolicy
    ) where

import           Oscoin.Crypto.Hash (HasHashing, Hash, compactHash)
import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
                 ( Beneficiary
                 , Block
                 , BlockHeader
                 , Height
                 , Sealed
                 , blockHash
                 , blockHeader
                 , blockHeight
                 , blockPrevHash
                 )
import           Oscoin.P2P as P2P
import           Oscoin.Protocol.Trace as Telemetry
import           Oscoin.Storage.Block.Abstract
                 (BlockStoreReader, getTip, lookupBlockByHeight)
import           Oscoin.Telemetry.Events (NotableEvent(NodeSyncEvent))
import           Oscoin.Telemetry.Trace as Telemetry
import           Oscoin.Time (Duration)

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.List (unfoldM)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import           Data.Numbers.Fibonacci (fib)
import           Formatting.Buildable (Buildable)
import           GHC.Natural

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type LocalTip c tx s  = Block c tx (Sealed c s)

type RemoteTip c tx s = Block c tx (Sealed c s)

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
    | BlockNotFound ByteString
      -- ^ The block with the given hash couldn't be downloaded.
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
    | GetBlockAtHeight
    -- ^ Get the block at a particular height.
    | GetBlock
    -- ^ Get the block identified by a given 'Hash'.
    | GetBlockHashes
    -- ^ Get some block hashes.
    | GetBlocks
    -- ^ Get some blocks.
    | GetBlockHeaders
    -- ^ Get some block headers.
    deriving (Show, Eq)

-- | The singleton type for 'ProtocolRequest', capable of carrying some
-- witness (in this case, 'ProtocolRequest').
data SProtocolRequest r where
  SGetTip           :: SProtocolRequest 'GetTip
  SGetBlockAtHeight :: SProtocolRequest 'GetBlockAtHeight
  SGetBlock         :: SProtocolRequest 'GetBlock
  SGetBlockHashes   :: SProtocolRequest 'GetBlockHashes
  SGetBlocks        :: SProtocolRequest 'GetBlocks
  SGetBlockHeaders  :: SProtocolRequest 'GetBlockHeaders

-- | A closed type family mapping each 'ProtocolRequest' to its list of
-- arguments, so that we can use the 'ProtocolRequest' (where all constructors
-- have kind /*/) as a constraint.
type family ProtocolRequestArgs c (r :: ProtocolRequest) :: * where
    ProtocolRequestArgs c 'GetTip           = ()
    ProtocolRequestArgs c 'GetBlockAtHeight = Height
    ProtocolRequestArgs c 'GetBlock         = Hash c
    ProtocolRequestArgs c 'GetBlockHashes   = Range
    ProtocolRequestArgs c 'GetBlocks        = Range
    ProtocolRequestArgs c 'GetBlockHeaders  = Range

-- | A protocol response, indexed by the corresponding 'ProtocolRequest, so
-- that matching the wrong type will result in a type error.
type family ProtocolResponse c (m :: * -> *) tx s (r :: ProtocolRequest) :: * where
    ProtocolResponse c m tx s 'GetTip =
        Block c tx (Sealed c s)
    ProtocolResponse c m tx s 'GetBlockAtHeight =
        Maybe (Block c tx (Sealed c s))
    ProtocolResponse c m tx s 'GetBlock =
        Block c tx (Sealed c s)
    ProtocolResponse c m tx s 'GetBlockHashes =
        ConduitT () (Hash c) m ()
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
            -> ProtocolRequestArgs c r
            -> m (Either SyncError (ProtocolResponse c m tx s r))
                }

-- | An opaque reference to a record of functions which can be used to query
-- the network and the other parts of the system for information.
data SyncContext c tx s m = SyncContext
    { scNu               :: Natural
    -- ^ Used to determine the block tolerance to consider
    -- ourselves synced. It correspond to /nu/ from the spec, and is used
    -- in the calculation of 'isDone'.
    , scMaxHashesPerPeer  :: Int
    -- ^ How many hashes we request each peer to download at any given time.
    -- This limits the number of information we have to exchange. A good value
    -- is usually between 50 and 500.
    , scBackwardDownloadThreshold :: Int
    -- ^ If the algorithm cannot reach full consensus on the next block to
    -- download (cfr 'downloadHashes') it transitions into an active phase
    -- where it tries to download blocks from the remote tip back to the
    -- local tip. Doing so in an unbound manner is dangerous as it might
    -- generate a lot of orphans, saturating the process' memory. This
    -- parameter establishes a threshold above which the algorithm won't try
    -- to download the blocks.
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
    , scDelay :: Int -> m ()
    -- ^ Waits for the number of given microseconds.
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

{------------------------------------------------------------------------------
                            Agreeing on a common height

The syncing process starts by calling 'commonChainHeight' to retrieve the
height associated to the block which root hash matches across all peers,
including the local node. To understand why this is important, let's take a
look at this picture, which shows 3 nodes at different heights and on different
chains:


                    height
                               +-----+
              node1   0 -------|-----|----------------------- 100
                               |     |
                               |     |
                               |     |
                               |     |
                               |     |     70
              node2   0 -------|-----|------\
                               |     |       \
                               |     |        \
                               |     |         --------- 80
                               |     |
                               |    -|-- 60
                               |   / |
              node3   0 -------|--/  |
                               | 55  |
                               +-----+

By returning 55 (perhaps inclusive of the block and/or the hash associated
with it) we are sure we are starting from a valid block.

-}

-- | Calculates the \"common chain height\" between this node and its active
-- peers. In case consensus can't be reached, the function retries again with
-- a fibonacci sequence, i.e. by requesting @local_tip - fib(n)@ up to a
-- limit.
-- Returns 0 (i.e. genesis' height) if no common height could be retrieved.
commonChainHeight
    :: forall c tx s m.
       ( Monad m
       , Ord (Hash c)
       )
    => Height
    -> SyncT c tx s m Height
commonChainHeight = commonChainHeightWith defaultRetryPolicy
  where
      defaultBacktrackPolicy :: BacktrackPolicy
      defaultBacktrackPolicy = fib

      -- By default, we retry 5 times in case the peers do not respond for
      -- any reason. We use 20 for 'rpMaxBacktrack' as in production, with
      -- a 'fib' policy, we would try to go back /at max/ fib(20) blocks
      -- (~ 6000).
      defaultRetryPolicy :: RetryPolicy
      defaultRetryPolicy = RetryPolicy defaultBacktrackPolicy 0 20 0 5

-- | A 'BacktrackPolicy' specifies how much we can jump backward when trying
-- to calculate the 'commonChainHeight'. For tests it's quite convenient to
-- use a 'linearBacktrackPolicy' as it allows for predictable tests, whereas
-- for production use something like a Fibonacci sequence works better.
type BacktrackPolicy = Int -> Int

linearBacktrackPolicy :: BacktrackPolicy
linearBacktrackPolicy = succ

data RetryPolicy = RetryPolicy
    { rpBacktrackPolicy :: BacktrackPolicy
    , rpBacktracks      :: Int
    -- ^ How much we have backtracked so far.
    , rpMaxBacktrack    :: Int
    -- ^ A threshold to limit the number of attempts when backtracking.
    , rpCurrentRetries  :: Int
    -- ^ How many times we have retried already.
    , rpMaxRetries      :: Int
    -- ^ How many retries we are allowed, at maximum.
    }

data SyncHeightResult =
      NobodyHasBlock
      -- ^ Nobody had the block
    | BlockNotFoundLocally
      -- ^ The local node doesn't have the block we downloaded from its peers.
    | BlockIsNotTheSameForAll
      -- ^ The block is not the same for all peers.
    | SyncHeightFailed

-- | Allows the 'commonChainHeight' algorithm to be passed a 'RetryPolicy'
-- which determines how far we need to backtrack when calculating the new
-- 'Height'.
commonChainHeightWith
    :: forall c tx s m.
       ( Monad m
       , Ord (Hash c)
       )
    => RetryPolicy
    -> Height
    -> SyncT c tx s m Height
commonChainHeightWith currentPolicy localHeight =
    if done
       then pure 0 -- returns genesis' height.
       else do
            res <- go currentPolicy
            case res of
              Left newPolicy     -> commonChainHeightWith newPolicy localHeight
              Right commonHeight -> pure commonHeight
  where

    -- We stop trying if the height is <= 0 or if we exceeded the backtrack
    -- limit.
    done :: Bool
    done = localHeight <= 0 ||
           rpBacktracks currentPolicy > rpMaxBacktrack currentPolicy

    go :: RetryPolicy -> SyncT c tx s m (Either RetryPolicy Height)
    go policy@RetryPolicy{..} = do
        ctx <- ask
        -- The current height to try is calculated from the initial, starting
        -- height (which never changes) minus the factor computed by the
        -- 'rpBacktrackPolicy'.
        let currentHeight =
                localHeight - fromIntegral (rpBacktrackPolicy rpBacktracks)
        -- Standard case: we received a response from our peers, so we simply
        -- increment the number of backtracks.
        let policy' = policy { rpBacktracks = rpBacktracks + 1 }

        res <- trySyncHeight currentHeight
        case res of
            Right ()                     -> pure $ Right currentHeight
            Left NobodyHasBlock          -> pure $ Left policy'
            Left BlockNotFoundLocally    -> pure $ Left policy'
            Left BlockIsNotTheSameForAll -> pure $ Left policy'
            -- Either we have some timeouts or the number of blocks received is
            -- insufficient. In this case we have to try again without
            -- increasing 'rpBacktracks'.
            Left SyncHeightFailed | rpCurrentRetries == rpMaxRetries ->
                -- In this case we want to try again but increasing the
                -- backtrack window but resetting the 'rpCurrentRetries'.
                pure $ Left $ policy' { rpCurrentRetries = 0 }
            Left SyncHeightFailed -> do
                -- In this case we do not want to modify the 'rpBacktracks',
                -- but simply try again, hoping the peers will reply to us
                -- eventually.
                lift $ scDelay ctx 1_000_000 -- wait 1 second
                pure . Left $ policy { rpCurrentRetries = rpCurrentRetries + 1 }

    -- Tries to fetch the block corresponding to the given 'Height' and
    -- it succeeds if the latter correspond to a block which is the same
    -- across all peers.
    trySyncHeight :: Height -> SyncT c tx s m (Either SyncHeightResult ())
    trySyncHeight requestedHeight = withActivePeers $ \ active -> do
        ctx <- ask
        results  <- lift $
            scConcurrently ctx (HS.toList active) (\a ->
                fetch (fetcher ctx) a SGetBlockAtHeight requestedHeight)
        case second catMaybes (partitionEithers results) of
          (_, []) -> pure $ Left NobodyHasBlock
          ([], blocks@(x:_)) | length blocks == HS.size active -> do
              mbBlock <- lift $ lookupBlockByHeight (localChain ctx) requestedHeight
              case mbBlock of
                Nothing -> pure $ Left BlockNotFoundLocally
                Just myBlock ->
                    if blockHash myBlock == blockHash x && allMatches (map blockHash blocks)
                       then pure $ Right ()
                       else pure $ Left BlockIsNotTheSameForAll
          _ -> pure $ Left SyncHeightFailed


    fetcher :: SyncContext c tx s m -> DataFetcher c tx s m
    fetcher = scDataFetcher

    localChain :: SyncContext c tx s m -> BlockStoreReader c tx s m
    localChain = scLocalChainReader

-- | Returns 'True' if all the elements of the list are the same.
-- In order to assess they all matches, we zip the original list with its
-- /reverse/, and check each pair.
allMatches :: Eq a => [a] -> Bool
allMatches [] = False
allMatches l =
    foldl' (\acc -> (&&) acc . uncurry (==)) True (zip l (reverse l))

{------------------------------------------------------------------------------
                            Syncing blocks
------------------------------------------------------------------------------}

-- | Given a starting height and a remote tip, syncs the blocks within that
-- range.
syncBlocks
    :: forall c tx s m.
       ( Monad m
       , Ord (Hash c)
       , Buildable (Hash c)
       , HasHashing c
       )
    => Height
    -- ^ The height the local node is starting from
    -> RemoteTip c tx s
    -- ^ The remote tip.
    -> SyncT c tx s m ()
syncBlocks startingHeight remoteTip = do
    ctx <- ask

    lastCommonBlock <- runConduit $
             C.yieldMany (hashRanges ctx)
          .| downloadHashes
          .| checkHashes
          .| downloadBlock
          .| notifyUpstreamConsumers
          .| C.foldl (\_acc b -> Just b) Nothing

    -- Stage2: If we didn't reach the target height, we download all the
    -- remaining blocks from remoteTip down to the last block we fetched.
    -- N.B. If we are too far away from the remote tip, it's pointless
    -- downloading these blocks, as they won't be applied anyway, they will
    -- simply be put in the orphanage without a good one.
    when (sufficientlyCloseToTip ctx lastCommonBlock) $

      -- The target height in the 'takeWhile' is just the @startingHeight@,
      -- because 'sufficientlyCloseToTip' enforces that this is < than
      -- 'scBackwardDownloadThreshold'.

      runConduit $
             unfoldM downloadBlocks (blockHash remoteTip)
          .| C.takeWhile (\b -> height b > startingHeight)
          .| notifyUpstreamConsumers
          .| C.sinkNull

  where

    remoteHeight :: Height
    remoteHeight = height remoteTip

    -- The gap between the remote and the starting tip.
    heightGap :: Height
    heightGap = remoteHeight - startingHeight

    -- Returns 'True' if we are \"sufficiently\" close to tip and we can
    -- try download blocks newest-to-oldest.
    sufficientlyCloseToTip
        :: SyncContext c tx s m
        -> Maybe (Block c tx (Sealed c s))
        -> Bool
    sufficientlyCloseToTip ctx commonBlock =
         (map blockHash commonBlock /= Just (blockHash remoteTip))
      && (heightGap <= fromIntegral (scBackwardDownloadThreshold ctx))


    -- Returns a list of ranges we can use to iterate through the ranges of
    -- the hashes we need to fetch, in steps of 'scMaxHashesPerPeer' at the time.
    -- If we have a @startingHeight@ of 50 and a remoteTip's height of 2000,
    -- we would get:
    -- >> hashRanges
    -- [Range 51 550, Range 551 1050, Range 1051 1550, Range 1551 2000]
    hashRanges :: SyncContext c tx s m -> [Range]
    hashRanges (fromIntegral . scMaxHashesPerPeer -> step) =
        let start = startingHeight
            end   = remoteHeight
            ana b = if b <= end then Just (Range (b + 1) (min (b + step) end), b + step)
                                else Nothing
        in unfoldr ana start

    -- Downloads some hashes and zip them together before propagating them
    -- downstream. This ensures that:
    -- 1. The conduit drains as soon as one of the returned lists is shorter
    --    than the rest (short-circuiting);
    -- 2. We are guaranteed that if a 'Hash' is propagated, it's shared
    --    between all the peers.
    downloadHashes
        :: ConduitT Range (Int, [Hash c]) (SyncT c tx s m) ()
    downloadHashes = awaitForever $ \rng -> do
        (peersNum, results) <- lift . withActivePeers $ \peers -> do
            forConcurrently <- asks scConcurrently
            dataFetcher     <- asks scDataFetcher
            (HS.size peers,) <$>
                lift (forConcurrently (HS.toList peers) (\a -> fetch dataFetcher a SGetBlockHashes rng))
        case partitionEithers results of
          -- Nobody has these hashes, we have to stop.
          (_, [])         -> pure ()

          -- Somebody has the hashes, but that's not enough.
          (_:_, _)        -> pure ()

          -- No errors, we can proceed
          ([], allHashes) ->
                mapOutput (peersNum,)
              . transPipe (SyncT . lift . lift)
              . mapInput (const ()) (const (Just rng))
              $ sequenceSources allHashes

    -- Check that all the hashes matches and that their number is equal to
    -- the number of active peers we fetched this information from.
    checkHashes
        :: ConduitT (Int, [Hash c]) (Hash c) (SyncT c tx s m) ()
    checkHashes = awaitForever $ \case
        (_, []) -> pure ()
        (expectedResponses, hashes@(x:_)) ->
            when (length hashes == expectedResponses && allMatches hashes) $
                yield x

    -- Downloads a block and yields it downstream.
    downloadBlock
        :: ConduitT (Hash c) (Block c tx (Sealed c s)) (SyncT c tx s m)( )
    downloadBlock = awaitForever $ \blockHash ->
        lift (downloadFromAny blockHash) >>= yield

    -- Download a 'Block' from /any/ peer, i.e. in FIFO fashion.
    downloadFromAny
        :: Hash c
        -> SyncT c tx s m (Block c tx (Sealed c s))
    downloadFromAny blockHash = withActivePeers $ \peers -> do
         fetcher <- asks scDataFetcher
         mbBlock <- foldM (\mbBlock peer ->
                      case mbBlock of
                        Just b  -> pure (Just b)
                        Nothing -> do
                            res <- lift $ fetch fetcher peer SGetBlock blockHash
                            case res of
                              Left _err -> pure Nothing -- TODO(adn) telemetry
                              Right b   -> pure $ Just b
                          ) Nothing (HS.toList peers)
         case mbBlock of
           Nothing ->
              throwError $ BlockNotFound (compactHash blockHash)
           Just b  -> pure b

    -- Download a single block from one of the available peers. The shape of
    -- this function is 'unfold' friendly, where the first element of the tuple
    -- is the downloaded block and the second its parent, i.e. the next
    -- element to download.
    downloadBlocks
        :: Hash c
        -> SyncT c tx s m (Maybe (Block c tx (Sealed c s), Hash c))
    downloadBlocks blockHash = do
         block <- downloadFromAny blockHash
         pure $ Just (block, blockPrevHash . blockHeader $ block)


    -- Notifies the upstream consumers about the downloaded blocks, and
    -- forward the missing blocks downstream.
    notifyUpstreamConsumers
        :: ConduitT (Block c tx (Sealed c s)) (Block c tx (Sealed c s)) (SyncT c tx s m) ()
    notifyUpstreamConsumers = awaitForever $ \incomingBlock -> do
        ctx <- lift ask
        lift $ do
            forM_ (scEventHandlers ctx) $ \dispatch ->
                lift $ dispatch (SyncBlock incomingBlock)
            recordEvent $
                Telemetry.NodeSyncDispatched (blockHash incomingBlock)
        yield incomingBlock -- propagate the block down.


-- | Calls 'isDone' internally, and try syncing blocks if it returns 'False'.
-- Returns the up-to-date local tip together with the remote tip fetched at
-- the time this function was first called.
syncUntil
    :: forall c tx s m.
       ( Monad m
       , Ord tx
       , Ord s
       , Ord (Beneficiary c)
       , Buildable (Hash c)
       , HasHashing c
       )
    => (SyncContext c tx s m -> LocalTip c tx s -> RemoteTip c tx s -> Bool)
    -- ^ A predicate on the local & remote tips.
    -> SyncT c tx s m (LocalTip c tx s, RemoteTip c tx s)
syncUntil finished = do
    ctx <- ask
    remoteTip    <- getRemoteTip
    localTip     <- lift (getTip (scLocalChainReader ctx))
    startHeight  <- commonChainHeight (height localTip)
    unlessDone ctx localTip remoteTip $
        syncBlocks startHeight remoteTip

  where
    unlessDone
        :: SyncContext c tx s m
        -> Block c tx (Sealed c s)
        -> Block c tx (Sealed c s)
        -> SyncT c tx s m ()
        -> SyncT c tx s m (LocalTip c tx s, RemoteTip c tx s)
    unlessDone ctx localTip remoteTip action =
        if finished ctx localTip remoteTip
           then do
               recordEvent $
                 Telemetry.NodeSyncFinished (blockHash localTip, height localTip)
               pure (localTip, remoteTip)
        else do
            recordEvent $
                Telemetry.NodeSyncStarted (blockHash localTip, height localTip)
                                          (blockHash remoteTip, height remoteTip)

            action

            newLocalTip <- lift (getTip (scLocalChainReader ctx))
            recordEvent $
                Telemetry.NodeSyncFinished (blockHash newLocalTip, height newLocalTip)
            pure (newLocalTip, remoteTip)

sync
    :: ( Monad m
       , Ord tx
       , Ord s
       , Ord (Beneficiary c)
       , Buildable (Hash c)
       , HasHashing c
       )
    => SyncT c tx s m ()
sync = void $ syncUntil (\ctx lcl rmt -> isDone (scNu ctx) lcl rmt)


{------------------------------------------------------------------------------
  Record telemetry events
------------------------------------------------------------------------------}

recordEvent
    :: ( Monad m
       , Buildable (Hash c)
       )
    => Telemetry.NodeSyncEvent c
    -> SyncT c tx s m ()
recordEvent evt = do
    ctx <- ask
    lift $ recordEvent' (scEventTracer ctx) evt

recordEvent'
    :: Buildable (Hash c)
    => Tracer m
    -> Telemetry.NodeSyncEvent c
    -> m ()
recordEvent' eventTracer evt =
    eventTracer $ Telemetry.traced (NodeSyncEvent evt) ()
