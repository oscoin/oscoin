{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

-- | Implementation of the \"Epidemic Broadcast Trees\" (aka \"Plumtree\")
-- gossip broadcast protocol.
--
-- <http://asc.di.fct.unl.pt/~jleitao/pdf/srds07-leitao.pdf>
module Oscoin.P2P.Gossip.Broadcast
    ( MessageId
    , Round

    , Meta (..)
    , IHave (..)
    , Gossip (..)
    , Message (..)
    , Outgoing (..)
    , ApplyResult (..)

    , Callbacks (..)

    , Handle
    , HasHandle (..)
    , hSelf
    , new

    , PlumtreeT
    , runPlumtreeT

    , Schedule
    , newSchedule
    , destroySchedule
    , withSchedule
    , schedule

    , eagerPushPeers
    , lazyPushPeers
    , resetPeers

    , broadcast
    , receive
    , neighborUp
    , neighborDown
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM
import           Control.Exception.Safe (bracket)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import qualified Focus
import           Lens.Micro (Lens', lens, over, set)
import           Lens.Micro.Mtl (view)
import qualified ListT
import           Oscoin.Clock
import qualified STMContainers.Map as STMMap

type MessageId = ByteString
type Round     = Word

data Meta n = Meta
    { metaMessageId :: MessageId
    , metaRound     :: Round
    , metaSender    :: n
    } deriving (Eq, Generic)

instance Serialise n => Serialise (Meta n)
instance Hashable  n => Hashable  (Meta n)

data IHave n = IHave (Meta n)
    deriving (Eq, Generic)

instance Serialise n => Serialise (IHave n)
instance Hashable  n => Hashable  (IHave n)

data Gossip n = Gossip
    { gPayload :: ByteString
    , gMeta    :: Meta n
    } deriving (Eq, Generic)

instance Serialise n => Serialise (Gossip n)
instance Hashable  n => Hashable  (Gossip n)

data Message n =
      GossipM (Gossip n)
    | IHaveM  (IHave n)
    | Prune   n
    | Graft   (Meta n)
    deriving (Eq, Generic)

instance Serialise n => Serialise (Message n)
instance Hashable  n => Hashable  (Message n)

data Outgoing n =
      Eager  n   (Message n)
    -- ^ Send 'Message n' to recipient 'n' immediately.
    | Lazy   n   (IHave n)
    -- ^ Send 'IHave n' to recipient 'n' according to some lazy queueing policy.
    | After  Int MessageId (IO [Outgoing n])
    -- ^ Run the action keyed by 'MessageId' after a timeout.
    | Cancel     MessageId
    -- ^ Cancel the 'After' action identified by 'MessageId'.
    --
    -- This is an optimisation allowing to cancel and GC any threads. When this
    -- is emitted, the internal state the corresponding 'After' action operates
    -- on will have been reset such that the action is a no-op.

-- | Result of 'applyMessage'.
data ApplyResult =
      Applied
    -- ^ The message was applied successfully, and was not known before.
    | Stale
    -- ^ The message was either applied before, or a later message rendered it
    -- obsolete.
    | Error
    -- ^ An error occurred. Perhaps the message was invalid.

data Callbacks = Callbacks
    { applyMessage  :: MessageId -> ByteString -> IO ApplyResult
    -- ^ Apply the payload associated with the given 'MessageId' to the local
    -- state.
    , lookupMessage :: MessageId -> IO (Maybe ByteString)
    -- ^ Retrieve the message payload for the given 'MessageId', or 'Nothing' if
    -- the message with that 'MessageId' has not been applied before.
    }

data Handle n = Handle
    { hSelf           :: n
    -- ^ Identity of this node.
    , hEagerPushPeers :: TVar (HashSet n)
    -- ^ The peers to eager push to.
    , hLazyPushPeers  :: TVar (HashSet n)
    -- ^ The peers to lazy push to.
    , hMissing        :: TVar (HashMap MessageId (IHave n))
    -- ^ Received 'IHave's for which we haven't requested the value yet.
    , hCallbacks      :: Callbacks
    -- ^ 'Callbacks' interface
    }

class HasHandle a n | a -> n where
    handle :: Lens' a (Handle n)

instance HasHandle (Handle n) n where
    handle = identity
    {-# INLINE handle #-}

hSelfL :: HasHandle a n => Lens' a n
hSelfL = handle . lens hSelf (\s a -> s { hSelf = a })
{-# INLINE hSelfL #-}

hEagerPushPeersL :: HasHandle a n => Lens' a (TVar (HashSet n))
hEagerPushPeersL = handle . lens hEagerPushPeers (\s a -> s { hEagerPushPeers = a })
{-# INLINE hEagerPushPeersL #-}

hLazyPushPeersL :: HasHandle a n => Lens' a (TVar (HashSet n))
hLazyPushPeersL = handle . lens hLazyPushPeers (\s a -> s { hLazyPushPeers = a })
{-# INLINE hLazyPushPeersL #-}

new :: (Eq n, Hashable n) => n -> Callbacks -> IO (Handle n)
new self cbs =
    Handle self <$> newTVarIO mempty
                <*> newTVarIO mempty
                <*> newTVarIO mempty
                <*> pure cbs

type PlumtreeT n r m a = ReaderT r m a

runPlumtreeT :: r -> PlumtreeT n r m a -> m a
runPlumtreeT = flip runReaderT

data Schedule n = Schedule
    { schedLazyQueue  :: STMMap.Map n (HashSet (IHave n))
    , schedLazyThread :: Async ()
    , schedDeferred   :: STMMap.Map MessageId [Async ()]
    -- TODO(kim): batch 'Message'
    , schedSend       :: n -> Message n -> IO ()
    }

newSchedule
    :: (Eq n, Hashable n)
    => Duration
    -> (n -> Message n -> IO ())
    -> IO (Schedule n)
newSchedule interval schedSend = do
    schedLazyQueue  <- STMMap.newIO
    schedDeferred   <- STMMap.newIO
    schedLazyThread <-
        async . forever $ do
            threadDelay $ round (interval `as` Microseconds)
            ihaves <-
                atomically $ do
                    ihaves <- ListT.toList $ STMMap.stream schedLazyQueue
                    STMMap.deleteAll schedLazyQueue
                    pure ihaves
            for_ (map (second (Set.map IHaveM)) ihaves) $ \(rcpt, ms) ->
                traverse_ (schedSend rcpt) ms

    pure Schedule{..}

destroySchedule :: Schedule n -> IO ()
destroySchedule Schedule{schedDeferred, schedLazyThread} = do
    deferreds <-
        atomically
            . ListT.fold (\xs -> pure . (xs <>) . snd) []
            $ STMMap.stream schedDeferred
    traverse_ Async.uninterruptibleCancel deferreds
    Async.uninterruptibleCancel schedLazyThread

withSchedule
    :: (Eq n, Hashable n)
    => Duration
    -> (n -> Message n -> IO ())
    -> (Schedule n -> IO a)
    -> IO a
withSchedule interval send k =
    bracket (newSchedule interval send) destroySchedule k

schedule :: (Eq n, Hashable n) => Schedule n -> Outgoing n -> IO ()
schedule sched@Schedule{..} = \case
    Eager r m -> schedSend r m
    Lazy  r i ->
        atomically $
            let update = pure . Just . maybe (Set.singleton i) (Set.insert i)
             in STMMap.focus (Focus.alterM update) r schedLazyQueue

    Cancel id -> do
        deferreds <-
            atomically $
                STMMap.focus (pure . (,Focus.Remove)) id schedDeferred
        (traverse_ . traverse_) Async.cancel deferreds

    After t id ma -> do
        ma' <-
            async $ do
                threadDelay (t * 1000000)
                ma >>= traverse_ (schedule sched)
        atomically $
            let update = pure . Just . maybe [ma'] (ma':)
             in STMMap.focus (Focus.alterM update) id schedDeferred

eagerPushPeers :: (MonadIO m, HasHandle r n) => PlumtreeT n r m (HashSet n)
eagerPushPeers = view hEagerPushPeersL >>= liftIO . readTVarIO

lazyPushPeers :: (MonadIO m, HasHandle r n) => PlumtreeT n r m (HashSet n)
lazyPushPeers = view hLazyPushPeersL >>= liftIO . readTVarIO

resetPeers
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => HashSet n
    -> PlumtreeT n r m ()
resetPeers peers = do
    Handle { hSelf           = self
           , hEagerPushPeers = eagers
           , hLazyPushPeers  = lazies
           } <- view handle
    liftIO . atomically $ do
        modifyTVar' eagers $ const (Set.delete self peers)
        modifyTVar' lazies $ const mempty

-- | Broadcast some arbitrary data to the network.
--
-- The caller is responsible for applying the message to the local state, such
-- that subsequent 'lookupMessage' calls would return 'Just' the 'ByteString'
-- passed in here. Correspondingly, subsequent 'applyMessage' calls must return
-- 'Stale' (or 'ApplyError').
broadcast
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => MessageId
    -> ByteString
    -> PlumtreeT n r m [Outgoing n]
broadcast mid msg = do
    self <- view hSelfL
    push Gossip
        { gPayload = msg
        , gMeta    = Meta
           { metaMessageId = mid
           , metaSender    = self
           , metaRound     = 0
           }
        }

-- | Receive and handle some 'Message' from the network.
receive
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => Message n
    -> PlumtreeT n r m [Outgoing n]
receive (GossipM g) = do
    Handle { hSelf = self
           , hMissing = missing
           , hCallbacks = Callbacks{applyMessage}
           } <- view handle

    let sender = view (gMetaL . metaSenderL)    g
    let mid    = view (gMetaL . metaMessageIdL) g

    r <- liftIO $ applyMessage mid (gPayload g)
    case r of
        Applied -> do
            -- Cancel any timers for this message.
            liftIO . atomically . modifyTVar' missing $ Map.delete mid
            out <-
                map (Cancel mid:) $
                    -- Disseminate gossip.
                    push . set  (gMetaL . metaSenderL) self
                         . over (gMetaL . metaRoundL)  (+1)
                         $ g
            moveToEager sender
            pure out
            -- Optimization (Section 3.8)
            -- Left out for now, as it's unclear what the value of 'threshold'
            -- should be. Riak also doesn't use this.
            {-
            outstanding <- Map.lookup mid <$> io (readTVarIO missing)
            pure $ case outstanding of
                Nothing           -> []
                Just (IHave meta) ->
                    let round  = view (gMetaL . metaRoundL) g
                        round' = metaRound meta
                     in if round' < round || round' - 1 >= threshold then
                            [ Eager (metaSender meta) $ Graft meta
                                { metaSender = self
                                , metaMessageId = ??
                                ]
                            , Eager sender $ Prune self
                            ]
                        else
                            []
            -}

        Stale -> do
            moveToLazy sender
            liftIO . atomically . modifyTVar' missing $ Map.delete mid
            pure $ [Eager sender $ Prune self]

        Error ->
            -- TODO(kim): log this
            pure mempty

receive (IHaveM ihave@(IHave Meta{metaMessageId = mid})) = do
    Handle { hMissing   = missing
           , hCallbacks = Callbacks{lookupMessage}
           } <- view handle

    msg <- liftIO $ lookupMessage mid
    case msg of
        Just _  -> pure mempty
        Nothing -> do
            liftIO . atomically . modifyTVar' missing $ Map.insert mid ihave
            scheduleGraft mid

receive (Prune sender) =
    moveToLazy sender $> mempty

receive (Graft meta@Meta{metaSender = sender, metaMessageId = mid}) = do
    Handle{hSelf = self, hCallbacks = Callbacks{lookupMessage}} <- view handle

    moveToEager sender

    payload <- liftIO $ lookupMessage mid
    pure $ case payload of
        Nothing -> mempty
        Just  p ->
            [ Eager sender $ GossipM Gossip
                { gPayload = p
                , gMeta    = meta { metaSender = self }
                }
            ]

-- | Peer sampling service callback when a new peer is detected.
--
-- Section 3.6, \"Dynamic Membership\":
--
-- \"When a new member is detected, it is simply added to the set of
-- eagerPushPeers, i.e. it is considered as a candidate to become part of the
-- tree.\".
neighborUp
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => n
    -> PlumtreeT n r m ()
neighborUp n = do
    eagers <- view hEagerPushPeersL
    liftIO . atomically . modifyTVar' eagers $ Set.insert n

-- | Peer sampling service callback when a peer leaves the overlay.
--
-- Section 3.6, \"Dynamic Membership\":
--
-- \"When a neighbor is detected to leave the overlay, it is simple[sic!]
-- removed from the membership. Furthermore, the record of 'IHave' messages sent
-- from failed members is deleted from the missing history.\"
neighborDown
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => n
    -> PlumtreeT n r m ()
neighborDown n = do
    Handle { hEagerPushPeers = eagers
           , hLazyPushPeers  = lazies
           , hMissing        = missing
           } <- view handle

    liftIO . atomically $ do
        modifyTVar' eagers  $ Set.delete n
        modifyTVar' lazies  $ Set.delete n
        modifyTVar' missing $ Map.filter (\(IHave meta) -> metaSender meta /= n)

-- Internal --------------------------------------------------------------------

scheduleGraft
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => MessageId
    -> PlumtreeT n r m [Outgoing n]
scheduleGraft mid = do
    hdl <- view handle
    pure [timer hdl timeout1 $ Just (timer hdl timeout2 Nothing)]
  where
    timeout1 = 5 * 1000000
    timeout2 = 1 * 1000000

    timer :: (Eq n, Hashable n) => Handle n -> Int -> Maybe (Outgoing n) -> Outgoing n
    timer hdl@Handle{hSelf = self, hMissing = missing} timeout k =
        After timeout mid $ do
            ann <-
                atomically $ do
                    ann <- Map.lookup mid <$> readTVar missing
                    modifyTVar' missing $ Map.delete mid
                    pure ann

            grf <-
                for ann $ \(IHave meta) ->
                    let sender = metaSender meta
                        graft  = Graft meta { metaSender = self }
                     in runPlumtreeT hdl (moveToEager sender)
                     $> Eager sender graft

            pure $ catMaybes [grf, k]

push
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => Gossip n
    -> PlumtreeT n r m [Outgoing n]
push g = do
    hdl <- view handle
    liftA2 (<>) (eagerPush hdl) (lazyPush hdl)
  where
    sender = view (gMetaL . metaSenderL) g

    eagerPush Handle{hEagerPushPeers = eagers} =
        let out to = Eager to (GossipM g)
         in map out . Set.toList . Set.delete sender <$> liftIO (readTVarIO eagers)

    lazyPush Handle{hSelf = self, hLazyPushPeers = lazies} =
        let out to = Lazy to $ IHave (gMeta g) { metaSender = self }
         in map out . Set.toList . Set.delete sender <$> liftIO (readTVarIO lazies)

-- Helpers ---------------------------------------------------------------------

moveToLazy
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => n
    -> PlumtreeT n r m ()
moveToLazy peer = do
    hdl <- view handle
    liftIO $ updatePeers hdl Set.delete Set.insert peer

moveToEager
    :: ( MonadIO m
       , HasHandle r n
       , Eq          n
       , Hashable    n
       )
    => n
    -> PlumtreeT n r m ()
moveToEager peer = do
    hdl <- view handle
    liftIO $ updatePeers hdl Set.insert Set.delete peer

updatePeers
    :: Eq n
    => Handle n
    -> (n -> HashSet n -> HashSet n)
    -> (n -> HashSet n -> HashSet n)
    -> n
    -> IO ()
updatePeers Handle { hSelf           = self
                   , hEagerPushPeers = eagers
                   , hLazyPushPeers  = lazies
                   }
            updateEager
            updateLazy
            peer
    | self == peer = pure ()
    | otherwise    = atomically $ do
        modifyTVar' eagers $ updateEager peer
        modifyTVar' lazies $ updateLazy  peer

-- Lenses ----------------------------------------------------------------------

metaMessageIdL :: Lens' (Meta n) MessageId
metaMessageIdL = lens metaMessageId (\s a -> s { metaMessageId = a })

metaRoundL :: Lens' (Meta n) Round
metaRoundL = lens metaRound (\s a -> s { metaRound = a })

metaSenderL :: Lens' (Meta n) n
metaSenderL = lens metaSender (\s a -> s { metaSender = a })

gMetaL :: Lens' (Gossip n) (Meta n)
gMetaL = lens gMeta (\s a -> s { gMeta = a })
