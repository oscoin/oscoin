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
    , new

    , Plumtree
    , runPlumtree

    , broadcast
    , receive
    , neighborUp
    , neighborDown
    ) where

import           Oscoin.Prelude hiding (Error)

import           Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
    | After  Int MessageId (Plumtree n [Outgoing n])
    -- ^ Run the action keyed by 'MessageId' after a timeout.
    | Cancel     MessageId
    -- ^ Cancel the 'After' action identified by 'MessageId'.

-- | Result of 'applyMessage'.
data ApplyResult =
      Applied -- ^ The message was applied successfully, and was not known before.
    | Stale   -- ^ The message was either applied before, or a later message rendered it obsolete.
    | Error   -- ^ An error occurred. Perhaps the message was invalid.

data Callbacks n = Callbacks
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
    , hEagerPushPeers :: TVar (Set n)
    -- ^ The peers to eager push to.
    , hLazyPushPeers  :: TVar (Set n)
    -- ^ The peers to lazy push to.
    , hMissing        :: TVar (Map MessageId (IHave n))
    -- ^ Received 'IHave's for which we haven't requested the value yet.
    , hCallbacks      :: Callbacks n
    -- ^ 'Callbacks' interface.
    }

new :: Ord n => n -> Set n -> Callbacks n -> IO (Handle n)
new self peers callbacks =
    Handle self <$> newTVarIO peers
                <*> newTVarIO mempty
                <*> newTVarIO mempty
                <*> pure callbacks

type Plumtree n = ReaderT (Handle n) IO

runPlumtree :: Handle n -> Plumtree n a -> IO a
runPlumtree r s = runReaderT s r

-- | Broadcast some arbitrary data to the network.
--
-- The caller is responsible for applying the message to the local state, such
-- that subsequent 'lookupMessage' calls would return 'Just' the 'ByteString'
-- passed in here. Correspondingly, subsequent 'applyMessage' calls must return
-- 'Stale' (or 'Error').
broadcast :: Ord n => MessageId -> ByteString -> Plumtree n [Outgoing n]
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
receive :: Ord n => Message n -> Plumtree n [Outgoing n]
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
            moveToEager sender
            -- Cancel any timers for this message.
            io . atomically . modifyTVar' missing $ Map.delete mid
            map (Cancel mid:) $
                push . set  (gMetaL . metaSenderL) self
                     . over (gMetaL . metaRoundL)  (+1)
                     $ g
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
                            , Not sender $ Prune self
                            ]
                        else
                            []
            -}

        Stale -> do
            moveToLazy sender
            io . atomically . modifyTVar' missing $ Map.delete mid
            pure $ [Eager sender $ Prune self]

        Error ->
            -- TODO(kim): log this
            pure mempty

receive (IHaveM ihave@(IHave Meta{metaMessageId = mid})) = do
    Handle { hMissing   = missing
           , hCallbacks = Callbacks{lookupMessage}
           } <- ask

    msg <- io $ lookupMessage mid
    case msg of
        Nothing -> pure mempty
        Just _  -> do
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
        Nothing -> []
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
neighborUp :: Ord n => n -> Plumtree n ()
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
neighborDown :: Ord n => n -> Plumtree n ()
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

scheduleGraft :: Ord n => MessageId -> Plumtree n [Outgoing n]
scheduleGraft mid = pure [timer timeout1 $ Just (timer timeout2 Nothing)]
  where
    timeout1 = 5 * 1000000
    timeout2 = 1 * 1000000

    timer timeout k = After timeout mid $ do
        Handle{hSelf = self, hMissing = missing} <- ask
        ann <-
            io . atomically $ do
                (ann, missing') <-
                        Map.updateLookupWithKey (\_ _ -> Nothing) mid
                    <$> readTVar missing
                writeTVar missing missing'
                pure ann

        grf <-
            for ann $ \(IHave meta) ->
                let sender = metaSender meta
                    graft  = Graft meta { metaSender = self }
                  in moveToEager sender $> Eager sender graft

        pure $ catMaybes [grf, k]

push :: Ord n => Gossip n -> Plumtree n [Outgoing n]
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

moveToLazy :: Ord n => n -> Plumtree n ()
moveToLazy peer = do
    hdl <- ask
    io $ updatePeers hdl Set.delete Set.insert peer

moveToEager :: Ord n => n -> Plumtree n ()
moveToEager peer = do
    hdl <- ask
    io $ updatePeers hdl Set.insert Set.delete peer

updatePeers :: Handle n -> (n -> Set n -> Set n) -> (n -> Set n -> Set n) -> n -> IO ()
updatePeers Handle{hEagerPushPeers = eagers, hLazyPushPeers = lazies}
            updateEager updateLazy peer =
    atomically $ do
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
