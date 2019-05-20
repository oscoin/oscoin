{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Protocol.Sync
    (
    -- * Types
      Sync(..)
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
    , range
    , isDone

    -- * Syncing functions
    , syncBlocks

    -- * Testing internals
    , withActivePeers
    , withActivePeer
    , getRemoteTip
    , getRemoteHeight
    ) where

import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Prelude
import           Prelude (last)

import           Oscoin.Consensus.Nakamoto (PoW)

import           Oscoin.Crypto.Blockchain.Block
                 (Block, BlockHeader, Height, Sealed, blockHeader, blockHeight)
import           Oscoin.Data.Tx
import           Oscoin.P2P as P2P
import           Oscoin.Storage.Block.Abstract (BlockStoreReader)
import           Oscoin.Telemetry.Trace
import           Oscoin.Time (Duration)
import           Oscoin.Time.Chrono

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.List ((\\))
import           Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           GHC.Natural

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type ActivePeer c  = NodeInfo c
type ActivePeers c = HashSet (ActivePeer c)

data SyncEvent c tx s =
      SyncBlock (Block c tx (Sealed c s))
    | SyncBlockHeader (BlockHeader c s)

deriving instance ( Eq (Hash c)
                  , Eq tx
                  , Eq s
                  , Eq (PublicKey c)
                  ) => Eq (SyncEvent c tx s)
deriving instance ( Ord (Hash c)
                  , Ord tx
                  , Ord s
                  , Ord (PublicKey c)
                  ) => Ord (SyncEvent c tx s)
deriving instance ( Show (Hash c)
                  , Show tx
                  , Show s
                  , Show (PublicKey c)
                  ) => Show (SyncEvent c tx s)

data SyncError =
      RequestTimeout ProtocolRequest Addr Timeout
      -- ^ The given peer exceeded the request timeout when serving this
      -- 'SyncProtocolRequest'.
    | NoActivePeers
      -- ^ There are not active peers to talk to.
    | NoRemoteTipFound
      -- ^ It was not possible to fetch a valid tip from our peers.
    | AllPeersTimeoutError
      -- ^ All the queried peers didn't respond in the allocated timeout.
    deriving (Show, Eq)

instance Exception SyncError

data Timeout =
    MaxTimeoutExceeded (Expected Duration)
    -- ^ The operation exceeded the expected timeout, in nanoseconds.
    deriving (Show, Eq)

-- | A closed range [lo,hi].
data Range = Range
    { start :: Height
    , end   :: Height
    } deriving Show

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
    , scActivePeers       :: m (ActivePeers c)
    , scDataFetcher       :: DataFetcher c tx s m
    , scEventTracer       :: Tracer m
    , scConcurrently      :: forall a b t.  Traversable t => t a -> (a -> m b) -> m (t b)
    , scLocalChainReader  :: BlockStoreReader c tx s m
    , scUpstreamConsumers :: [SyncEvent c tx s -> m ()]
    }

-- | A sync monad over @m@, that can throw 'SyncError's.
newtype Sync c tx s m a =
    Sync { runSync :: ExceptT SyncError (ReaderT (SyncContext c tx s m) m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError SyncError
             , MonadReader (SyncContext c tx s m)
             )

instance MonadTrans (Sync c tx s) where
    lift = Sync . lift . lift

instance MonadIO m => MonadIO (Sync c tx s m) where
    liftIO = Sync . liftIO

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

-- | Like 'withActivePeers', but it passes to the callback only a (single)
-- active peer. This might be useful in tests which it's not necessary to have
-- all the active peers available.
withActivePeer
    :: Monad m
    => (ActivePeer c -> Sync c tx s m a)
    -> Sync c tx s m a
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

-- | The nu parameter, from the spec.
type Nu = Natural

maxBlocksPerPeer :: Int
maxBlocksPerPeer = 50

-- | Returns 'True' if the node finished syncing all the blocks.
-- This is the 'isDone' state transition function from the spec.
isDone
    :: ( Eq tx
       , Eq s
       , Eq (Hash c)
       , Eq (PublicKey c)
       )
    => Nu
    -> Block c tx s
    -> Block c tx s
    -> Bool
isDone (fromIntegral -> nu) localTip remoteTip =
    remoteTip == localTip || height remoteTip - height localTip < nu

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
       ( ProtocolResponse c tx s 'GetTip ~ Block c tx (Sealed c s)
       , Monad m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (PublicKey c)
       ) => Sync c tx s m (ProtocolResponse c tx s 'GetTip)
getRemoteTip = withActivePeers $ \active -> do
    dataFetcher     <- scDataFetcher  <$> ask
    forConcurrently <- scConcurrently <$> ask
    results  <- lift $
        forConcurrently (HS.toList active) (\a -> fetch dataFetcher a SGetTip ())
    case partitionEithers results of
      (_t:_, [])           -> throwError AllPeersTimeoutError
      ([], [])             -> throwError NoRemoteTipFound
      (_timeouts, allTips) -> do
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
    :: ( ProtocolResponse c tx s 'GetTip ~ Block c tx (Sealed c s)
       , Monad m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (PublicKey c)
       )
    => Sync c tx s m Height
getRemoteHeight = map height getRemoteTip

-- | Given a valid 'Range', syncs the blocks within the range.
syncBlocks
    :: ( ProtocolResponse c tx s 'GetBlocks ~ OldestFirst [] (Block c tx (Sealed c s))
       , Monad m
       , Ord tx
       , Ord s
       , Ord (Hash c)
       , Ord (PublicKey c)
       )
    => Range
    -> Sync c tx s m ()
syncBlocks rng = do
    ctx <- ask
    let fetcher = scDataFetcher ctx
    withActivePeers $ \ activePeers -> do

        let blocksPerPeer =
                min maxBlocksPerPeer
                    (ceiling @Double @Int (fromIntegral (end rng - start rng) / fromIntegral (length activePeers)))

        -- Distribute work across peers, which is equally split, where
        -- each peer can be given @at most@ 'maxBlocksPerPeer' at the time,
        -- in order to limit the amount of blocks in transit each time.
        let workDistribution =
                zip (cycle (HS.toList activePeers))
                    (chunksOf blocksPerPeer [start rng .. end rng])

        -- FIXME(adn) streaming?
        (requestedBlocks, !missingBlocks) <- lift $
            foldM (\(requested, missing) (activePeer, !heightRange) -> do
                     case Range <$> head heightRange <*> pure (last heightRange) of
                         Nothing -> pure (requested, missing)
                         Just subRange -> do
                             res <- fetch fetcher activePeer SGetBlocks subRange
                             case res of
                               Left _timeout ->
                                   pure (requested, missing <> heightRange) -- TODO(adn) Telemetry
                               Right blks    -> do
                                   -- If the peer didn't have the requested range,
                                   -- the list of missing blocks increases.
                                   -- FIXME(adn) not even trying to be efficient
                                   -- in this first version.
                                   let missingHeights = heightRange \\ map height (toOldestFirst blks)
                                   pure $ ( foldl' (flip Set.insert) requested (toOldestFirst blks)
                                          , missing <> missingHeights
                                          )
                  )
                  (mempty, [])
                  workDistribution

        forM_ requestedBlocks $ \b ->
            forM_ (scUpstreamConsumers ctx) $ \dispatch ->
                lift $ dispatch (SyncBlock b)

        -- Handle missing blocks
        -- NOTE(adn) Not terribly convinced about this optimization.
        case sort missingBlocks of
          [] -> pure ()
          stillMissing -> forM_ stillMissing $ \missing -> do
              -- Tries to download each individual block from each and every
              -- peer. We don't fail in case the block cannot be downloaded,
              -- as we hope that eventually the peers will be able to catch up.
              results <- forM (HS.toList activePeers) $ \peer -> do
                           res <- lift $ fetch fetcher peer SGetBlocks (Range missing missing)
                           case res of
                             Left _err -> pure Nothing
                             Right oldest -> case toOldestFirst oldest of
                                               [b] -> pure (Just b)
                                               _   -> pure Nothing
              case catMaybes results of
                []  -> pure () -- TODO(adn) telemetry?
                b:_ ->
                    forM_ (scUpstreamConsumers ctx) $ \dispatch ->
                        lift $ dispatch (SyncBlock b)
