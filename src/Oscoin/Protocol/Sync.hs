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
    , getRemoteTip
    , getRemoteHeight
    ) where

import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Prelude

import           Oscoin.Consensus.Nakamoto (PoW)

import           Oscoin.Crypto.Blockchain.Block
                 (Block, BlockHeader, Height, Sealed, blockHeader, blockHeight)
import           Oscoin.Data.Tx
import           Oscoin.P2P as P2P
import           Oscoin.Storage.Block.Abstract (BlockStoreReader)
import           Oscoin.Telemetry.Trace
import           Oscoin.Time (Duration)
import           Oscoin.Time.Chrono

import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
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

{------------------------------------------------------------------------------
  Pure functions
------------------------------------------------------------------------------}

height :: Block c tx s -> Height
height = blockHeight . blockHeader

-- | The nu parameter, from the spec.
type Nu = Natural

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

syncBlocks
    :: ( ProtocolResponse c tx s 'GetBlocks ~ OldestFirst [] (Block c tx (Sealed c s))
       , Hashable (Block c tx (Sealed c s))
       , Monad m
       , Eq tx
       , Eq s
       , Eq (Hash c)
       , Eq (PublicKey c)
       )
    => Range
    -> Sync c tx s m ()
syncBlocks rng = do
    ctx <- ask
    let fetcher = scDataFetcher ctx
    withActivePeers $ \ activePeers -> do
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

      -- FIXME(adn) streaming?
      forM_ (HS.toList requestedBlocks) $ \b ->
          forM_ (scUpstreamConsumers ctx) $ \dispatch ->
              lift $ dispatch (SyncBlock b)

