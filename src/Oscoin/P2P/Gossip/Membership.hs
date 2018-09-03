-- | Implementation of the HyParView membership protocol.
--
-- <http://asc.di.fct.unl.pt/~jleitao/pdf/dsn07-leitao.pdf>
--
-- The types are parametrised over a node identity @n@. Note that @n@ must carry
-- enough information to be able to establish a physical network connection (see
-- 'connOpen'). In order to impede both sybil and eclipse attacks, it is
-- advisable to also factor in a cryptographic node id. How to do that is beyond
-- the scope of this module.
module Oscoin.P2P.Gossip.Membership
    ( Peers (active, passive)

    , Callbacks (..)
    , getPeers

    , Handle
    , hSelf
    , new

    , Config (..)
    , defaultConfig

    , RPC (..)
    , Message (..)
    , Priority (..)

    , HyParView
    , runHyParView

    , getPeers'
    , activeView
    , passiveView

    , receive
    , eject
    , joinAny
    , joinFirst
    , shuffle
    , promoteRandom
    ) where

import           Oscoin.Prelude

import           Oscoin.P2P.Gossip.Membership.Internal

import           Control.Concurrent.STM.TVar
import           Control.Exception.Safe (SomeException, handleAny, throwM, tryAny)
import           Control.Monad (when)
import           Data.Bool (bool)
import           Data.Foldable (foldlM)
import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Maybe (listToMaybe, maybeToList)
import           Data.Traversable (for)
import           System.Random (randomR, split)
import           System.Random.SplitMix (SMGen)

data Callbacks n = Callbacks
    { -- | Called when a node gets added to the active view.
      --
      -- Part of the \"Peer Sampling Service\" interface as specified by the
      -- plumtree paper.
      neighborUp   :: n -> IO () -- FIXME: possibly not IO

      -- | Called when a node gets removed from the active view.
      --
      -- Part of the \"Peer Sampling Service\" interface as specified by the
      -- plumtree paper.
    , neighborDown :: n -> IO () -- FIXME: possibly not IO

      -- | Open a new physical connection to 'n'.
      --
      -- The connection must have reliable ordered semantics.
      --
      -- If the connection could not be established, this should throw an
      -- exception.
      --
      -- The implementor should keep track of open connections, and use an
      -- existing one if possible.
    , connOpen     :: n -> IO ()

      -- | Close a physical connection to 'n'.
      --
      -- This is mostly an optimisation to allow for prompt finalisation. Once
      -- this has been called, the connection must not be used to send or
      -- receive 'RPC's. Closing an already closed connection should have no
      -- effect. Exceptions raised by this action are ignored.
    , connClose    :: n -> IO ()
    }

data Peers n = Peers
    { active  :: HashSet n
    , passive :: HashSet n
    }

data Handle n = Handle
    { hSelf      :: n
    , hConfig    :: Config
    , hPRNG      :: IORef SMGen
    , hCallbacks :: Callbacks n
    , hActive    :: TVar (HashSet n)
    , hPassive   :: TVar (HashSet n)
    }

new :: (Eq n, Hashable n) => n -> Config -> SMGen -> Callbacks n -> IO (Handle n)
new hSelf hConfig prng hCallbacks = do
    hPRNG    <- newIORef prng
    hActive  <- newTVarIO mempty
    hPassive <- newTVarIO mempty
    pure Handle {..}

data Config = Config
    { -- | Maximum size of the active view.
      cfgMaxActive      :: Word8
      -- | Maximum size of the passive view.
    , cfgMaxPassive     :: Word8
      -- | Active Maximum Random Walk Length.
      --
      -- Specifies the maximum number of hops a 'ForwardJoin' rpc is propagated.
    , cfgARWL           :: Word8
      -- | Passive Maximum Random Walk Length.
      --
      -- Specifies the number of hops at which a node propagated via
      -- 'ForwardJoin' is inserted into the passive view.
    , cfgPRWL           :: Word8
      -- | Number of nodes from the active view to include in a 'Shuffle'
      -- request.
      --
      -- @ka@ in the paper.
    , cfgShuffleActive  :: Word8
      -- | Number of nodes from the passive view to include in a 'Shuffle'
      -- request.
      --
      -- @kp@ in the paper.
    , cfgShufflePassive :: Word8
    }

-- | Default 'Config' with values used for the paper.
defaultConfig :: Config
defaultConfig = Config
    { cfgMaxActive      = 5
    , cfgMaxPassive     = 30
    , cfgARWL           = 6
    , cfgPRWL           = 3
    , cfgShuffleActive  = 3
    , cfgShufflePassive = 4
    }

data RPC n = RPC
    { rpcSender    :: n
    , rpcRecipient :: n
    , rpcPayload   :: Message n
    }

data Message n =
      Join
    | ForwardJoin     n TimeToLive
    | Disconnect
    | Neighbor        Priority
    | NeighborReject
    | Shuffle         n (HashSet n) TimeToLive
    | ShuffleReply    (HashSet n)

data Priority = Low | High
    deriving (Eq, Ord, Show)

type HyParView n = ReaderT (Handle n) IO

runHyParView :: Handle n -> HyParView n a -> IO a
runHyParView r ma = runReaderT ma r

-- | Obtain the current active view.
--
-- This is part of the \"Peer Sampling Service\" interface as specified by the
-- plumtree paper.
getPeers :: HyParView n (HashSet n)
getPeers = activeView

-- | Obtain a consistent snapshot of both the active and passive view.
getPeers' :: HyParView n (Peers n)
getPeers' = do
    Handle{hActive, hPassive} <- ask
    io . atomically $
        liftA2 Peers (readTVar hActive) (readTVar hPassive)

-- | Obtain a snapshot of the active view.
activeView :: HyParView n (HashSet n)
activeView = asks hActive >>= io . readTVarIO

-- | Obtain a snapshot of the passive view.
passiveView :: HyParView n (HashSet n)
passiveView = asks hPassive >>= io . readTVarIO

-- | Handle an incoming 'RPC' and return a (possibly empty) list of outgoing
-- 'RPC's.
--
-- Rethrows any exceptions raised by attempting to establish new connections.
receive :: (Eq n, Hashable n) => RPC n -> HyParView n [RPC n]
receive RPC{rpcSender, rpcPayload} = case rpcPayload of
    Join -> do
        arwl <- asks $ fromIntegral . cfgARWL . hConfig
        liftA2 (<>)
               (addToActive' rpcSender)
               (broadcast $ ForwardJoin rpcSender arwl)

    ForwardJoin joining ttl -> do
        self  <- asks hSelf
        nactv <- numActive
        if joining /= self && (isExpired ttl || nactv == 1) then do
            -- Notify the joining node of the endpoint of the random walk by
            -- sending it a 'Neighbor' request. This is an omission in the
            -- paper: the joining node cannot otherwise learn about having been
            -- added to some other node's active view, violating the
            -- symmetricity property of the active views.
            prio <- neighborPriority
            liftA2 (:)
                   (send joining $ Neighbor prio)
                   (addToActive' joining)
        else do
            prwl <- asks $ fromIntegral . cfgPRWL . hConfig
            when (ttl == prwl) $
                addToPassive joining
            let fwd = ForwardJoin joining (decr ttl)
            maybeToList <$> sendAnyActive fwd [joining]

    Disconnect -> do
        removeFromActive rpcSender >>= traverse_ closeConnection
        addToPassive rpcSender
        -- Crucial omission in the paper: if the network starts to 'Disconnect'
        -- us, we must actively seek to remain connected.
        nactv <- numActive
        if nactv > 1 then
            pure mempty
        else do
            -- Try to promote a random passive node.
            rpn <- randomPassiveNodeNot rpcSender
            case rpn of
                -- Note: 'High' priority to ensure this actually succeeds (the
                -- same node could have rejected us before).
                Just n  -> liftA2 (:) (send n $ Neighbor High) (addToActive' n)
                -- We ran out of passive nodes, ask the network for some fresh
                -- ones.
                Nothing -> maybeToList <$> shuffle

    Neighbor prio -> do
        full <- ask >>= io . atomically . isActiveAtCapacity
        if prio == High || not full then
            addToActive' rpcSender
        else
            singleton <$> reply NeighborReject

    NeighborReject ->
        -- Deviation from the paper: we do _not_ add the rejecting node to the
        -- passive view here, as this can lead to a "death loop", where we just
        -- cycle through a set of nodes likely to reject us again. Instead, in
        -- 'eject', we initiate a 'shuffle' if we run out of peers (either
        -- active or passive) to increase our options.
        eject rpcSender

    Shuffle origin nodes ttl -> do
        nactv <- numActive
        let ttl' = decr ttl
        if not (isExpired ttl') && nactv > 1 then
            maybeToList <$> sendAnyActive (Shuffle origin nodes ttl') mempty
        else do
            rpns <- randomPassiveNodes (Set.size nodes)
            addAllToPassive nodes
            singleton <$> send origin (ShuffleReply rpns)

    ShuffleReply nodes -> do
        addAllToPassive nodes
        -- Emergency: we may have issued a shuffle because we where running low
        -- on peers. Try to promote a random passive node if that's the case.
        nactv <- numActive
        if nactv <= 1 then promoteRandom else pure mempty
  where
    singleton x = [x]

    reply = send rpcSender

    sendAnyActive msg omit =
        traverse (`send` msg) =<< randomActiveNodeNot (Set.fromList (rpcSender:omit))

    broadcast msg = do
        actv <- asks hActive >>= io . readTVarIO
        traverse (`send` msg) . Set.toList $ Set.delete rpcSender actv

-- | Eject a node suspected to be faulty from the active view.
--
-- If the node was not in the active view, does nothing, otherwise, tries to
-- promote a random node from the passive view by attempting to establish a
-- connection to it. If that succeeds, send it a 'Neighbor' RPC, which may
-- eventually lead to the node being promoted to the active view of the peer
-- (see 'receive'). Otherwise, removes that node from the passive view and tries
-- another.
--
-- Note that the paper does not specify whether the ejecting node @p@ should add
-- the to-be-promoted node @q@ to it's active view immediately after
-- successfully establishing a connection, or wait for some confirmation from
-- @q@. We opt to add it to the active view optimistically, and eject it again
-- if we receive a 'NeighborReject' from it. Consequentially,
--
--   * the component handling the sending of the 'RPC's should call 'eject' if
--   that doesn't succeed (which should mean that it can treat all sending
--   uniformly)
--   * the receiver of a 'Neighbor' 'RPC' wishing to reject the request should
--   close the connection on its end after replying with 'NeighborReject', such
--   that the node gets 'eject'ed on the requestor's side on the next attempt to
--   use the connection.
eject :: (Eq n, Hashable n) => n -> HyParView n [RPC n]
eject n = do
    removeFromActive n >>= traverse_ closeConnection
    promoted <- promoteRandom
    nactv    <- numActive
    -- Crucial: initiate a 'shuffle' if we seem to run out of peers.
    if null promoted || nactv <= 2 then
        mappend promoted . maybeToList <$> shuffle
    else
        pure promoted

-- | Join the overlay by attempting to connect to the supplied contact nodes.
--
-- If the connection attempt succeeds, the contact node is added to the active
-- view, and a 'Join' rpc is placed in the returned list to be sent to that
-- contact.
--
-- All contacts (up to @cfgMaxActive + cfgMaxPassive@) will be tried,
-- left-to-right.
--
-- This should only be called once to boostrap the protocol, with empty 'Peers'.
-- This condition is, however, not checked nor enforced. Hence, the returned
-- list of 'RPC's may also contain 'Disconnect' messages due to evicted active
-- peers.
joinAny :: (Eq n, Hashable n) => [n] -> HyParView n [RPC n]
joinAny ns = do
    Config{cfgMaxActive, cfgMaxPassive} <- asks hConfig
    map join . traverse go $
        take (fromIntegral $ cfgMaxActive + cfgMaxPassive) ns
  where
    go n = do
        x <- tryAny $ addToActive' n
        case x of
            Right r -> (:r) <$> send n Join
            Left  _ -> pure []

-- | Like 'joinAny', but stop on the first successfully established connection.
joinFirst :: (Eq n, Hashable n) => [n] -> HyParView n [RPC n]
joinFirst []     = pure []
joinFirst (n:ns) = do
    x <- tryAny $ addToActive' n
    case x of
        Right r -> (:r) <$> send n Join
        Left  _ -> joinFirst ns

-- | Initiate a 'Shuffle'.
--
-- This is supposed to be called periodically in order to exchange peers with
-- the known part of the network.
--
-- Returns 'Nothing' if the active view is empty, 'Just' the 'Shuffle' 'RPC'
-- otherwise.
shuffle :: (Eq n, Hashable n) => HyParView n (Maybe (RPC n))
shuffle = do
    Config{cfgARWL=arwl, cfgShuffleActive=ka, cfgShufflePassive=kp} <-
        asks hConfig
    s   <- asks hSelf
    ran <- randomActiveNode
    for ran $ \r -> do
        as <- randomActiveNodesNot (Set.singleton r) (fromIntegral ka)
        ps <- randomPassiveNodes (fromIntegral kp)
        send r $ Shuffle s (as <> ps) (fromIntegral arwl)

--------------------------------------------------------------------------------

-- | Select a node from the active view at random.
--
-- If the active view is empty, 'Nothing' is returned, otherwise 'Just' the
-- random node.
randomActiveNode :: HyParView n (Maybe n)
randomActiveNode = do
    Handle{hPRNG, hActive} <- ask
    io $ do
        actv <- readTVarIO hActive
        prng <- atomicModifyIORef' hPRNG split
        pure . fst $ randomFromSet actv prng

-- | Select a node @n@, but not nodes @ns@, from the active view at random.
randomActiveNodeNot :: (Eq n, Hashable n) => HashSet n -> HyParView n (Maybe n)
randomActiveNodeNot ns = do
    Handle{hPRNG, hActive} <- ask
    io $ do
        actv <- readTVarIO hActive
        prng <- atomicModifyIORef' hPRNG split
        pure . fst $ randomFromSet (Set.difference actv ns) prng

-- | Select 'num' nodes, but not nodes 'ns', from the active view at random.
--
-- If the active view is empty, the empty set is returned. 'num' is adjusted to
-- be:
--
-- > min (size active - size omitted) (min cfgMaxActive num)
randomActiveNodesNot :: (Eq n, Hashable n) => HashSet n -> Int -> HyParView n (HashSet n)
randomActiveNodesNot ns num = do
    Handle{hConfig, hActive} <- ask
    actv <- io $ readTVarIO hActive
    let min' = min (Set.size actv - Set.size ns)
             . min num
             . fromIntegral . cfgMaxActive
             $ hConfig
    loop min' Set.empty
  where
    loop min' !s = do
        ran <- randomActiveNodeNot ns
        case ran of
            Nothing -> pure s
            Just n' | s' <- Set.insert n' s ->
                if Set.size s' >= min' then
                    pure s'
                else
                    loop min' s'

-- | Select a node from the passive view at random.
--
-- If the passive view is empty, 'Nothing' is returned, otherwise 'Just' the
-- random node.
randomPassiveNode :: HyParView n (Maybe n)
randomPassiveNode = do
    Handle{hPRNG, hPassive} <- ask
    io $ do
        pasv <- readTVarIO hPassive
        prng <- atomicModifyIORef' hPRNG split
        pure . fst $ randomFromSet pasv prng

-- | Select a node @n'@ from the passive view at random, such that @n' /= n@
randomPassiveNodeNot :: (Eq n, Hashable n) => n -> HyParView n (Maybe n)
randomPassiveNodeNot n = do
    Handle{hPRNG, hPassive} <- ask
    io $ do
        pasv <- readTVarIO hPassive
        prng <- atomicModifyIORef' hPRNG split
        pure . fst $ randomFromSet (Set.delete n pasv) prng

-- | Select 'num' nodes from the passive view at random.
--
-- If the passive view is empty, the empty set is returned. 'num' is adjusted to
-- be:
--
-- > min (size passive) (min cfgMaxPassive num)
randomPassiveNodes :: (Eq n, Hashable n) => Int -> HyParView n (HashSet n)
randomPassiveNodes num = do
    Handle{hConfig, hPRNG, hPassive} <- ask
    prng <- io $ atomicModifyIORef' hPRNG split
    io . atomically $ do
        pasv <- readTVar hPassive
        let min' = min (Set.size pasv)
                 . min num
                 . fromIntegral . cfgMaxPassive
                 $ hConfig
        loop min' pasv prng Set.empty
  where
    loop min' pasv prng !s =
        case randomFromSet pasv prng of
            (Nothing, _)     -> pure $! s
            (Just n,  prng') | s' <- Set.insert n s ->
                if Set.size s' >= min' then
                    pure $! s'
                else
                    loop min' pasv prng' s'

-- | Add a peer to the active view, removing a random other node from the active
-- view if it is full.
--
-- If the peer is in the passive view, it will get removed from there.
--
-- If a peer gets removed to make space for the new one, it is returned as a
-- 'Just', otherwise 'Nothing' is returned.
--
-- Before the peer is added to the active view, a connection is attempted to be
-- established via 'connOpen'. This may fail throwing an arbitrary exception, in
-- which case the peer is /not/ added to the active view. __The exception is
-- rethrown__
addToActive :: (Eq n, Hashable n) => n -> HyParView n (Maybe n)
addToActive n = ask >>= go
  where
    go hdl@Handle{hSelf, hPRNG, hActive, hPassive}
      | hSelf == n = pure Nothing
      | otherwise  = do
        conn <- tryAny $ openConnection n
        case conn of
            Left e   -> throwM e
            Right () -> do
                removed <- io $ do
                    gen <- atomicModifyIORef' hPRNG split
                    atomically $ do
                        (removed,_) <- evictActive hdl gen
                        modifyTVar' hActive  $ Set.insert n
                        modifyTVar' hPassive $ Set.delete n
                        pure removed
                notifyUp n
                for_ removed $ \rm'd -> do
                    closeConnection rm'd
                    notifyDown rm'd
                pure removed

-- | Add a peer to the passive view, removing a random other node from the
-- passive view if it is full.
addToPassive :: (Eq n, Hashable n) => n -> HyParView n ()
addToPassive n = do
    hdl@Handle{hPRNG} <- ask
    io $ do
        gen <- atomicModifyIORef' hPRNG split
        atomically . void $ addToPassive' hdl gen n

-- | Add a set of peers to the passive view.
--
-- This implements an optimisation: by first removing the given peers from the
-- passive view, we avoid having the repeatedly evict peers when the passive
-- view is at capacity.
addAllToPassive :: (Eq n, Hashable n) => HashSet n -> HyParView n ()
addAllToPassive ns = do
    hdl@Handle{hSelf = self, hPRNG, hActive, hPassive} <- ask
    io $ do
        gen <- atomicModifyIORef' hPRNG split
        atomically $ do
            actv <- readTVar hActive
            let ns' = Set.difference (Set.delete self ns) actv
            modifyTVar' hPassive $ (`Set.difference` ns')
            void $ foldlM (addToPassive' hdl) gen ns'

removeFromActive :: (Eq n, Hashable n) => n -> HyParView n (Maybe n)
removeFromActive n = do
    prev <- ask >>= io . atomically . flip removeFromActive' n
    traverse_ notifyDown prev
    pure prev

removeFromPassive :: (Eq n, Hashable n) => n -> HyParView n ()
removeFromPassive n = ask >>= io . atomically . flip removeFromPassive' n

-- | Attempt to promote a random passive node to the active view.
--
-- This should be called periodically at an interval smaller than the rate of
-- new nodes joining (which in turn should be rate-limited).
promoteRandom :: (Eq n, Hashable n) => HyParView n [RPC n]
promoteRandom = randomPassiveNode >>= maybe (pure []) promote
  where
    promote n' = do
        x <- tryAny $ addToActive' n'
        case x of
            Left  _ -> do
                -- TODO(kim): logging?
                removeFromPassive n'
                promoteRandom
            Right r -> do
                prio <- neighborPriority
                (:r) <$> send n' (Neighbor prio)

--------------------------------------------------------------------------------

notifyUp :: n -> HyParView n ()
notifyUp n = do
    up <- asks $ neighborUp . hCallbacks
    io $ up n

notifyDown :: n -> HyParView n ()
notifyDown n = do
    down <- asks $ neighborDown . hCallbacks
    io $ down n

--------------------------------------------------------------------------------

addToActive' :: (Eq n, Hashable n) => n -> HyParView n [RPC n]
addToActive' n =
    addToActive n >>= map maybeToList . traverse (`send` Disconnect)

numActive :: HyParView n Int
numActive = asks hActive >>= map Set.size . io . readTVarIO

send :: n -> Message n -> HyParView n (RPC n)
send to payload = do
    s <- asks hSelf
    pure RPC
        { rpcSender    = s
        , rpcRecipient = to
        , rpcPayload   = payload
        }

openConnection :: n -> HyParView n ()
openConnection to = do
    open <- asks $ connOpen . hCallbacks
    io $ open to

closeConnection :: n -> HyParView n ()
closeConnection to = do
    close <- asks $ connClose . hCallbacks
    handleAny ignoreException $ io (close to)

neighborPriority :: HyParView n Priority
neighborPriority = do
    actv <- asks hActive
    bool Low High . Set.null <$> io (readTVarIO actv)

--------------------------------------------------------------------------------

isActiveAtCapacity :: Handle n -> STM Bool
isActiveAtCapacity Handle{hConfig = Config{cfgMaxActive}, hActive} = do
    actv <- readTVar hActive
    pure $ Set.size actv >= maxActv
  where
    maxActv = fromIntegral cfgMaxActive

isPassiveAtCapacity :: Handle n -> STM Bool
isPassiveAtCapacity Handle{hConfig = Config{cfgMaxPassive}, hPassive} = do
    pasv <- readTVar hPassive
    pure $ Set.size pasv >= maxPasv
  where
    maxPasv = fromIntegral cfgMaxPassive

evictActive :: (Eq n, Hashable n) => Handle n -> SMGen -> STM (Maybe n, SMGen)
evictActive hdl@Handle{hActive} gen =
    isActiveAtCapacity hdl >>= bool (pure (Nothing, gen)) (evict hActive gen)

evictPassive :: (Eq n, Hashable n) => Handle n -> SMGen -> STM (Maybe n, SMGen)
evictPassive hdl@Handle{hPassive} gen =
    isPassiveAtCapacity hdl >>= bool (pure (Nothing, gen)) (evict hPassive gen)

evict :: (Eq n, Hashable n) => TVar (HashSet n) -> SMGen -> STM (Maybe n, SMGen)
evict ref gen = do
    s <- readTVar ref
    let rnd = randomFromSet s gen
    traverse_ (modifyTVar' ref . Set.delete) $ fst rnd
    pure rnd

addToPassive' :: (Eq n, Hashable n) => Handle n -> SMGen -> n -> STM SMGen
addToPassive' hdl@Handle{hSelf = self, hActive, hPassive} gen n = do
    actv <- readTVar hActive
    if n /= self && not (Set.member n actv) then do
        (_,gen') <- evictPassive hdl gen
        modifyTVar' hPassive $ Set.insert n
        pure gen'
    else
        pure gen

removeFromActive' :: (Eq n, Hashable n) => Handle n -> n -> STM (Maybe n)
removeFromActive' Handle{hActive} n = do
    actv <- readTVar hActive
    if Set.member n actv then do
        modifyTVar' hActive $ Set.delete n
        pure $ Just n
    else
        pure Nothing

removeFromPassive' :: (Eq n, Hashable n) => Handle n -> n -> STM ()
removeFromPassive' Handle{hPassive} n = modifyTVar' hPassive $ Set.delete n

--------------------------------------------------------------------------------

ignoreException :: (Applicative m, Monoid w) => SomeException -> m w
ignoreException = const $ pure mempty

randomFromSet :: HashSet a -> SMGen -> (Maybe a, SMGen)
randomFromSet s gen | Set.null s = (Nothing, gen)
                    | otherwise  =
    let (i, gen') = randomR (0, Set.size s - 1) gen
     in (listToMaybe . drop i $ Set.toList s, gen')
