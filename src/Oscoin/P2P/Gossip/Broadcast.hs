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
    , hSelf
    , new

    , Plumtree
    , runPlumtree

    , eagerPushPeers
    , lazyPushPeers

    , broadcast
    , receive
    , neighborUp
    , neighborDown
    ) where

import           Oscoin.Prelude

import           Control.Concurrent.STM
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Lens.Micro (Lens', lens, over, set)
import           Lens.Micro.Extras (view)

type MessageId = ByteString
type Round     = Word

data Meta n = Meta
    { metaMessageId :: MessageId
    , metaRound     :: Round
    , metaSender    :: n
    } deriving (Eq, Ord)

data IHave n = IHave (Meta n)
    deriving (Eq, Ord)

data Gossip n = Gossip
    { gPayload :: ByteString
    , gMeta    :: Meta n
    }

data Message n =
      GossipM (Gossip n)
    | IHaveM  (IHave n)
    | Prune   n
    | Graft   (Meta n)

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
    | ApplyError
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
    -- ^ 'Callbacks' interface.
    }

new :: (Eq n, Hashable n) => n -> HashSet n -> Callbacks -> IO (Handle n)
new self peers callbacks =
    Handle self <$> newTVarIO (Set.delete self peers)
                <*> newTVarIO mempty
                <*> newTVarIO mempty
                <*> pure callbacks

type Plumtree n = ReaderT (Handle n) IO

runPlumtree :: Handle n -> Plumtree n a -> IO a
runPlumtree r s = runReaderT s r

eagerPushPeers :: Plumtree n (HashSet n)
eagerPushPeers = asks hEagerPushPeers >>= io . readTVarIO

lazyPushPeers :: Plumtree n (HashSet n)
lazyPushPeers = asks hLazyPushPeers >>= io . readTVarIO

-- | Broadcast some arbitrary data to the network.
--
-- The caller is responsible for applying the message to the local state, such
-- that subsequent 'lookupMessage' calls would return 'Just' the 'ByteString'
-- passed in here. Correspondingly, subsequent 'applyMessage' calls must return
-- 'Stale' (or 'ApplyError').
broadcast :: (Eq n, Hashable n) => MessageId -> ByteString -> Plumtree n [Outgoing n]
broadcast mid msg = do
    self <- asks hSelf
    push Gossip
        { gPayload = msg
        , gMeta    = Meta
           { metaMessageId = mid
           , metaSender    = self
           , metaRound     = 0
           }
        }

-- | Receive and handle some 'Message' from the network.
receive :: (Eq n, Hashable n) => Message n -> Plumtree n [Outgoing n]
receive (GossipM g) = do
    Handle { hSelf      = self
           , hMissing   = missing
           , hCallbacks = Callbacks{applyMessage}
           } <- ask

    let sender = view (gMetaL . metaSenderL)    g
    let mid    = view (gMetaL . metaMessageIdL) g

    r <- io $ applyMessage mid (gPayload g)
    case r of
        Applied -> do
            -- Cancel any timers for this message.
            io . atomically . modifyTVar' missing $ Map.delete mid
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
            io . atomically . modifyTVar' missing $ Map.delete mid
            pure $ [Eager sender $ Prune self]

        ApplyError ->
            -- TODO(kim): log this
            pure mempty

receive (IHaveM ihave@(IHave Meta{metaMessageId = mid})) = do
    Handle { hMissing   = missing
           , hCallbacks = Callbacks{lookupMessage}
           } <- ask

    msg <- io $ lookupMessage mid
    case msg of
        Just _  -> pure mempty
        Nothing -> do
            io . atomically . modifyTVar' missing $ Map.insert mid ihave
            scheduleGraft mid

receive (Prune sender) =
    moveToLazy sender $> mempty

receive (Graft meta@Meta{metaSender = sender, metaMessageId = mid}) = do
    Handle { hSelf      = self
           , hCallbacks = Callbacks{lookupMessage}
           } <- ask

    moveToEager sender

    payload <- io $ lookupMessage mid
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
neighborUp :: (Eq n, Hashable n) => n -> Plumtree n ()
neighborUp n = do
    eagers <- asks hEagerPushPeers
    io . atomically . modifyTVar' eagers $ Set.insert n

-- | Peer sampling service callback when a peer leaves the overlay.
--
-- Section 3.6, \"Dynamic Membership\":
--
-- \"When a neighbor is detected to leave the overlay, it is simple[sic!]
-- removed from the membership. Furthermore, the record of 'IHave' messages sent
-- from failed members is deleted from the missing history.\"
neighborDown :: (Eq n, Hashable n) => n -> Plumtree n ()
neighborDown n = do
    Handle { hEagerPushPeers = eagers
           , hLazyPushPeers  = lazies
           , hMissing        = missing
           } <- ask

    io . atomically $ do
        modifyTVar' eagers  $ Set.delete n
        modifyTVar' lazies  $ Set.delete n
        modifyTVar' missing $ Map.filter (\(IHave meta) -> metaSender meta /= n)

-- Internal --------------------------------------------------------------------

scheduleGraft :: (Eq n, Hashable n) => MessageId -> Plumtree n [Outgoing n]
scheduleGraft mid = do
    hdl <- ask
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
                     in runPlumtree hdl (moveToEager sender)
                     $> Eager sender graft

            pure $ catMaybes [grf, k]

push :: (Eq n, Hashable n) => Gossip n -> Plumtree n [Outgoing n]
push g = do
    hdl <- ask
    liftA2 (<>) (eagerPush hdl) (lazyPush hdl)
  where
    sender = view (gMetaL . metaSenderL) g

    eagerPush Handle{hEagerPushPeers = eagers} =
        let out to = Eager to (GossipM g)
         in map out . Set.toList . Set.delete sender <$> io (readTVarIO eagers)

    lazyPush Handle{hSelf = self, hLazyPushPeers = lazies} =
        let out to = Lazy to $ IHave (gMeta g) { metaSender = self }
         in map out . Set.toList . Set.delete sender <$> io (readTVarIO lazies)

-- Helpers ---------------------------------------------------------------------

moveToLazy :: (Eq n, Hashable n) => n -> Plumtree n ()
moveToLazy peer = do
    hdl <- ask
    io $ updatePeers hdl Set.delete Set.insert peer

moveToEager :: (Eq n, Hashable n) => n -> Plumtree n ()
moveToEager peer = do
    hdl <- ask
    io $ updatePeers hdl Set.insert Set.delete peer

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
