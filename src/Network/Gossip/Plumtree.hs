-- | Implementation of the \"Epidemic Broadcast Trees\" (aka \"Plumtree\")
-- gossip broadcast protocol.
--
-- <http://asc.di.fct.unl.pt/~jleitao/pdf/srds07-leitao.pdf>
module Network.Gossip.Plumtree
    ( MessageId
    , Round

    , Meta (..)
    , IHave (..)
    , Gossip (..)
    , Message (..)
    , ApplyResult (..)

    , Handle
    , hSelf
    , new

    , PlumtreeC (..)

    , Plumtree
    , runPlumtree

    , eagerPushPeers
    , lazyPushPeers
    , resetPeers

    , broadcast
    , receive
    , neighborUp
    , neighborDown
    ) where

import           Prelude

import           Codec.Serialise (Serialise)
import           Control.Applicative (liftA2)
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Trans.Cont
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.Time.Clock (NominalDiffTime)
import           GHC.Generics (Generic)
import           Lens.Micro (Lens', lens, over, set)
import           Lens.Micro.Mtl (view)

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

-- | Result of 'applyMessage'.
data ApplyResult =
      Applied (Maybe MessageId)
    -- ^ The message was applied successfully, and was not known before. If
    -- 'Just' a 'MessageId' is given, it is indicated that an earlier message by
    -- that id was determined to be missing, and the network is instructed to
    -- attempt to retransmit it.
    | Stale (Maybe MessageId)
    -- ^ The message was either applied before, or a later message rendered it
    -- obsolete. Similar to 'Applied', 'Just' a 'MessageId' indicates that an
    -- earlier message should be retransmitted by the network.
    | Error
    -- ^ An error occurred. Perhaps the message was invalid.

data Handle n = Handle
    { hSelf           :: n
    -- ^ Identity of this node.
    , hEagerPushPeers :: TVar (HashSet n)
    -- ^ The peers to eager push to.
    , hLazyPushPeers  :: TVar (HashSet n)
    -- ^ The peers to lazy push to.
    , hMissing        :: TVar (HashMap MessageId (IHave n))
    -- ^ Received 'IHave's for which we haven't requested the value yet.
    }

new :: (Eq n, Hashable n) => n -> IO (Handle n)
new self =
    Handle self <$> newTVarIO mempty <*> newTVarIO mempty <*> newTVarIO mempty

-- Continuations ---------------------------------------------------------------

data PlumtreeC n a =
      ApplyMessage  MessageId       ByteString                  (ApplyResult      -> IO (PlumtreeC n a))
    | LookupMessage MessageId                                   (Maybe ByteString -> IO (PlumtreeC n a))
    | SendEager     n               (Message n)                 (IO (PlumtreeC n a))
    | SendLazy      n               (IHave   n)                 (IO (PlumtreeC n a))
    | Later         NominalDiffTime MessageId   (Plumtree n ()) (IO (PlumtreeC n a))
    | Cancel        MessageId                                   (IO (PlumtreeC n a))
    | Done          a

sendEager :: n -> Message n -> Plumtree n ()
sendEager to msg =
    Plumtree $ ReaderT $ \_ -> ContT $ \k ->
        pure $ SendEager to msg (k ())

sendLazy :: n -> IHave n -> Plumtree n ()
sendLazy to ihave =
    Plumtree $ ReaderT $ \_ -> ContT $ \k ->
        pure $ SendLazy to ihave (k ())

later :: NominalDiffTime -> MessageId -> Plumtree n () -> Plumtree n ()
later timeout key action =
    Plumtree $ ReaderT $ \_ -> ContT $ \k ->
        pure $ Later timeout key action (k ())

cancel :: MessageId -> Plumtree n ()
cancel key =
    Plumtree $ ReaderT $ \_ -> ContT $ \k ->
        pure $ Cancel key (k ())

applyMessage :: MessageId -> ByteString -> Plumtree n ApplyResult
applyMessage mid v =
    Plumtree $ ReaderT $ \_ -> ContT $ \k ->
        pure $ ApplyMessage mid v k

lookupMessage :: MessageId -> Plumtree n (Maybe ByteString)
lookupMessage mid =
    Plumtree $ ReaderT $ \_ -> ContT $ \k ->
        pure $ LookupMessage mid k

-- Monad -----------------------------------------------------------------------

newtype Plumtree n a = Plumtree
    { fromPlumtree :: forall x. ReaderT (Handle n) (ContT (PlumtreeC n x) IO) a
    } deriving Functor

instance Applicative (Plumtree n) where
    pure x = Plumtree $ pure x
    (<*>)  = ap

instance Monad (Plumtree n) where
    return            = pure
    Plumtree m >>= f = Plumtree $ m >>= fromPlumtree . f
    {-# INLINE (>>=) #-}

instance MonadIO (Plumtree n) where
    liftIO io = Plumtree $ liftIO io

instance MonadReader (Handle n) (Plumtree n) where
    ask       = Plumtree $ ReaderT pure
    local f m = Plumtree $ local f (fromPlumtree m)

runPlumtree :: Handle n -> Plumtree n a -> IO (PlumtreeC n a)
runPlumtree r (Plumtree ma) = runContT (runReaderT ma r) (pure . Done)

-- API -------------------------------------------------------------------------

eagerPushPeers :: Plumtree n (HashSet n)
eagerPushPeers = asks hEagerPushPeers >>= liftIO . readTVarIO

lazyPushPeers :: Plumtree n (HashSet n)
lazyPushPeers = asks hLazyPushPeers >>= liftIO . readTVarIO

resetPeers
    :: (Eq n, Hashable n)
    => HashSet n
    -> Plumtree n ()
resetPeers peers = do
    Handle { hSelf           = self
           , hEagerPushPeers = eagers
           , hLazyPushPeers  = lazies
           } <- ask
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
    :: (Eq n, Hashable n)
    => MessageId
    -> ByteString
    -> Plumtree n ()
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
receive :: (Eq n, Hashable n) => Message n -> Plumtree n ()
receive (GossipM g) = do
    Handle { hSelf = self
           , hMissing = missing
           } <- ask

    let sender = view (gMetaL . metaSenderL)    g
    let mid    = view (gMetaL . metaMessageIdL) g

    r <- applyMessage mid (gPayload g)
    case r of
        Applied retransmit -> do
            -- Cancel any timers for this message.
            liftIO . atomically . modifyTVar' missing $ Map.delete mid
            cancel mid
            -- Disseminate gossip.
            push . set  (gMetaL . metaSenderL) self
                 . over (gMetaL . metaRoundL)  (+1)
                 $ g
            moveToEager sender
            for_ retransmit $ \re ->
                let
                    gmeta = Meta
                          { metaMessageId = re
                          , metaRound     = 0
                          , metaSender    = self
                          }
                    graft = Graft gmeta
                 in
                    sendEager sender graft
            -- Nb. Optimization (Section 3.8) left out for now, as it's unclear
            -- what the value of 'threshold' should be. Riak also doesn't use
            -- this.

        Stale retransmit -> do
            liftIO . atomically . modifyTVar' missing $ Map.delete mid
            case retransmit of
                Nothing -> sendEager sender (Prune self)
                Just re ->
                    let
                        gmeta = Meta
                              { metaMessageId = re
                              , metaRound     = 0
                              , metaSender    = self
                              }
                        graft = Graft gmeta
                     in
                        sendEager sender graft
            moveToLazy sender

        Error ->
            -- TODO(kim): log this
            pure ()

receive (IHaveM ihave@(IHave Meta{metaMessageId = mid})) = do
    missing <- asks hMissing
    msg     <- lookupMessage mid
    case msg of
        Just _  -> pure ()
        Nothing -> do
            liftIO . atomically . modifyTVar' missing $ Map.insert mid ihave
            scheduleGraft mid

receive (Prune sender) =
    moveToLazy sender

receive (Graft meta@Meta{metaSender = sender, metaMessageId = mid}) = do
    self    <- asks hSelf
    payload <- lookupMessage mid
    moveToEager sender
    for_ payload $ \p ->
        sendEager sender $ GossipM Gossip
            { gPayload = p
            , gMeta    = meta { metaSender = self }
            }

-- | Peer sampling service callback when a new peer is detected.
--
-- Section 3.6, \"Dynamic Membership\":
--
-- \"When a new member is detected, it is simply added to the set of
-- eagerPushPeers, i.e. it is considered as a candidate to become part of the
-- tree.\".
neighborUp
    :: (Eq n, Hashable n)
    => n
    -> Plumtree n ()
neighborUp n = do
    eagers <- asks hEagerPushPeers
    liftIO . atomically . modifyTVar' eagers $ Set.insert n

-- | Peer sampling service callback when a peer leaves the overlay.
--
-- Section 3.6, \"Dynamic Membership\":
--
-- \"When a neighbor is detected to leave the overlay, it is simple[sic]
-- removed from the membership. Furthermore, the record of 'IHave' messages sent
-- from failed members is deleted from the missing history.\"
neighborDown
    :: (Eq n, Hashable n)
    => n
    -> Plumtree n ()
neighborDown n = do
    Handle { hEagerPushPeers = eagers
           , hLazyPushPeers  = lazies
           , hMissing        = missing
           } <- ask

    liftIO . atomically $ do
        modifyTVar' eagers  $ Set.delete n
        modifyTVar' lazies  $ Set.delete n
        modifyTVar' missing $ Map.filter (\(IHave meta) -> metaSender meta /= n)

-- Internal --------------------------------------------------------------------

scheduleGraft :: (Eq n, Hashable n) => MessageId -> Plumtree n ()
scheduleGraft mid =
    later timeout1 mid $ go (Just timeout2)
  where
    timeout1 = 5 * 1000000
    timeout2 = 1 * 1000000

    go next = do
        Handle { hSelf    = self
               , hMissing = missing
               } <- ask

        ann <-
            liftIO . atomically $ do
                ann <- Map.lookup mid <$> readTVar missing
                modifyTVar' missing $ Map.delete mid
                pure ann

        for_ ann $ \(IHave meta) -> do
            let sender = metaSender meta
            let graft  = Graft meta { metaSender = self }
            moveToEager sender
            sendEager sender graft

        for_ next $ \timeout ->
            later timeout mid $ go Nothing

push :: (Eq n, Hashable n) => Gossip n -> Plumtree n ()
push g = do
    Handle { hSelf           = self
           , hEagerPushPeers = eagers
           , hLazyPushPeers  = lazies
           } <- ask

    (eagers', lazies') <-
        liftIO . atomically $
            liftA2 (,)
                   (Set.delete sender <$> readTVar eagers)
                   (Set.delete sender <$> readTVar lazies)

    for_ eagers' $ \to ->
        sendEager to (GossipM g)

    for_ lazies' $ \to ->
        sendLazy to $ IHave (gMeta g) { metaSender = self }
  where
    sender = view (gMetaL . metaSenderL) g

-- Helpers ---------------------------------------------------------------------

moveToLazy :: (Eq n, Hashable n) => n -> Plumtree n ()
moveToLazy peer = do
    hdl <- ask
    liftIO $ updatePeers hdl Set.delete Set.insert peer

moveToEager :: (Eq n, Hashable n) => n -> Plumtree n ()
moveToEager peer = do
    hdl <- ask
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
