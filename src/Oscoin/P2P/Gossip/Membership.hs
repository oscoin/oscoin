-- | Implementation of the HyParView membership protocol.
--
-- <http://asc.di.fct.unl.pt/~jleitao/pdf/dsn07-leitao.pdf>
--
-- The types are parametrised over a node identity @n@, and a connection type
-- @c@ (specified by 'connOpen' and 'connClose'). Note that @n@ must carry
-- enough information to be able to establish a physical network connection (see
-- 'connOpen'). In order to impede both sybil and eclipse attacks, it is
-- advisable to also factor in a cryptographic node id.  How to do that is
-- beyond the scope of this module.
module Oscoin.P2P.Gossip.Membership
    ( Peers

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

    , receive
    , eject
    , joinAny
    , shuffle
    ) where

import           Oscoin.Prelude hiding (runStateT)

import           Oscoin.P2P.Gossip.Membership.Internal

import           Control.Exception.Safe (SomeException, handleAny, onException, tryAny)
import           Control.Monad (when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.State.Strict (StateT, gets, modify', runStateT, state)
import           Data.Bool (bool)
import           Data.Functor (($>))
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import qualified Data.Set as Set
import           Data.Traversable (for)
import           Lens.Micro (over)
import           System.Random.MWC (Gen, GenIO, uniformR)

data Callbacks n c = Callbacks
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

      -- | Open a new physical connection. Should use TCP as the underlying
      -- transport.
      --
      -- If the connection could not be established, this should throw an
      -- exception.
      --
      -- The implementor should keep track of open connections, and return an
      -- existing one if possible.
    , connOpen     :: n -> IO c

      -- | Close a connection.
      --
      -- This is mostly an optimisation to allow for prompt finalisation. Once
      -- this has been called, the connection must not be used to send or
      -- receive 'RPC's. Closing an already closed connection should have no
      -- effect. Exceptions raised by this action are ignored.
    , connClose    :: c -> IO ()
    }

data Handle n c = Handle
    { hSelf      :: n
    , hConfig    :: Config
    , hPRNG      :: GenIO
    , hCallbacks :: Callbacks n c
    }

new :: n -> Config -> GenIO -> Callbacks n c -> Handle n c
new = Handle

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
    | Shuffle         n (Set n) TimeToLive
    | ShuffleReply    (Set n)

data Priority = Low | High
    deriving (Eq, Ord, Show)

type HyParView n c = StateT (Peers n c) (ReaderT (Handle n c) IO)

runHyParView :: Handle n c -> Peers n c -> HyParView n c a -> IO (a, Peers n c)
runHyParView r s = flip runReaderT r . flip runStateT s

-- | Obtain the current active view.
--
-- This is part of the \"Peer Sampling Service\" interface as specified by the
-- plumtree paper.
getPeers :: HyParView n c (Map n c)
getPeers = gets active

-- | Handle an incoming 'RPC' and return a (possibly empty) list of outgoing
-- 'RPC's.
--
-- Rethrows any exceptions raised by attempting to establish new connections.
receive :: Ord n => RPC n -> HyParView n c [RPC n]
receive RPC{rpcSender, rpcPayload} = case rpcPayload of
    Join -> do
        arwl <- asks $ fromIntegral . cfgARWL . hConfig
        liftA2 (<>)
               (addToActive' rpcSender)
               (broadcast $ ForwardJoin rpcSender arwl)

    ForwardJoin joining ttl -> do
        actv <- gets active
        if isExpired ttl || Map.null actv then
            -- FIXME: this is as described in the paper, but violates the
            -- property that active views are symmetric - the 'joining' node is
            -- not aware of having been added to a node several hops away.
            -- Should we send a 'Neighbor' rpc back to 'joining'?
            addToActive' joining
        else do
            prwl <- asks $ fromIntegral . cfgPRWL . hConfig
            when (ttl == prwl) $
                addToPassive joining
            maybeToList <$> sendAnyActive (ForwardJoin joining (pred ttl))

    Disconnect -> do
        removeFromActive rpcSender >>= traverse_ closeConnection
        addToPassive rpcSender $> []

    Neighbor prio -> do
        full <- isActiveAtCapacity
        if prio == High || not full then
            addToActive' rpcSender
        else
            singleton <$> reply NeighborReject

    NeighborReject -> do
        -- Note: this may unnecessarily remove a node from the passive view.
        -- This seems acceptable, since the rejecting node is likely of high
        -- quality (ie. reachable, popular).
        addToPassive rpcSender
        eject rpcSender

    Shuffle origin nodes ttl -> do
        actv <- gets active
        let ttl' = pred ttl
        if ttl' > 0 && Map.size actv > 1 then
            maybeToList <$> sendAnyActive (Shuffle origin nodes ttl')
        else do
            rpns <- randomPassiveNodes (Set.size nodes)
            purgePassive nodes
            traverse_ addToPassive nodes
            singleton <$> send origin (ShuffleReply rpns)

    ShuffleReply ns -> do
        purgePassive ns
        traverse_ addToPassive ns $> []

  where
    singleton x = [x]

    reply             = send rpcSender
    sendAnyActive msg = traverse (`send` msg) =<< randomActiveNodeNot rpcSender
    broadcast     msg = traverse (`send` msg) .
        Map.keys . flip Map.withoutKeys (Set.singleton rpcSender) =<< gets active

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
-- if we receive a 'NeighborReject' from it. Consequentially:
--
--   * the component handling the sending of the 'RPC's should call 'eject' if
--   that doesn't succeed (which should mean that it can treat all sending
--   uniformly)
--   * the receiver of a 'Neighbor' 'RPC' wishing to reject the request should
--   close the connection on its end after replying with 'NeighborReject', such
--   that the node gets 'eject'ed on the requestor's side on the next attempt to
--   use the connection.
eject :: Ord n => n -> HyParView n c [RPC n]
eject n = do
    removed <- removeFromActive n
    traverse_ closeConnection removed
    case removed of
        Nothing -> pure []
        Just _  -> recurse
  where
    promote n' = do
        x <- tryAny $ addToActive' n'
        case x of
            Left  _ -> do
                removeFromPassive n'
                recurse
            Right r -> do
                prio <- bool Low High . Map.null <$> gets active
                (:r) <$> send n' (Neighbor prio)

    -- Note: we ensure here that the chosen passive node is not equal to the one
    -- we want to eject. This may seem redundant, but allows us to reuse this
    -- method in the 'NeighborReject' case, which demotes the rejecting node.
    recurse = randomPassiveNodeNot n >>= maybe (pure []) promote

-- | Join the overlay by attempting to connect to the supplied contact nodes.
--
-- If the connection attempt succeeds, the contact node is added to the active
-- view, and a 'Join' rpc is placed in the returned list to be sent to that
-- contact.
--
-- It is not guaranteed that all contacts will be tried, the implementation is
-- free to abort on the first successful.
--
-- This should only be called once to boostrap the protocol, with empty 'Peers'.
-- This condition is, however, not checked nor enforced. Hence, the returned
-- list of 'RPC's may also contain 'Disconnect' messages due to evicted active
-- peers.
joinAny :: Ord n => [n] -> HyParView n c [RPC n]
joinAny = map join . traverse go
  where
    go n = do
        x <- tryAny $ addToActive' n
        case x of
            Left  _ -> pure []
            Right r -> (:r) <$> send n Join

-- | Initiate a 'Shuffle'.
--
-- This is supposed to be called periodically in order to exchange peers with
-- the known part of the network.
--
-- Returns 'Nothing' if the active view is empty, 'Just' the 'Shuffle' 'RPC'
-- otherwise.
shuffle :: Ord n => HyParView n c (Maybe (RPC n))
shuffle = do
    Config{cfgARWL=arwl, cfgShuffleActive=ka, cfgShufflePassive=kp} <-
        asks hConfig
    s   <- asks hSelf
    ran <- randomActiveNode
    for ran $ \r -> do
        as <- randomActiveNodesNot r (fromIntegral ka)
        ps <- randomPassiveNodes (fromIntegral kp)
        send r $ Shuffle s (as <> ps) (fromIntegral arwl)

-- | Select a node from the active view at random.
--
-- If the active view is empty, 'Nothing' is returned, otherwise 'Just' the
-- random node.
randomActiveNode :: HyParView n c (Maybe n)
randomActiveNode = do
    prng <- asks hPRNG
    actv <- gets active
    randomKeyFromMap prng actv

-- | Select a node @n'@ from the active view at random, such that @n' /= n@
randomActiveNodeNot :: Ord n => n -> HyParView n c (Maybe n)
randomActiveNodeNot n = do
    prng <- asks hPRNG
    actv <- gets active
    randomKeyFromMap prng (Map.delete n actv)

-- | Select 'num' nodes, but not node 'n', from the active view at random.
--
-- If the active view is empty, the empty set is returned. 'num' is adjusted to
-- be:
--
-- > min (size active) (min cfgMaxActive num)
randomActiveNodesNot :: Ord n => n -> Int -> HyParView n c (Set n)
randomActiveNodesNot n num = do
    actv <- gets active
    min' <- min (Map.size actv) . min num . fromIntegral . cfgMaxActive
        <$> asks hConfig
    loop min' Set.empty
  where
    loop min' !s = do
        ran <- randomActiveNodeNot n
        case ran of
            Nothing -> pure s
            Just n' | s' <- Set.insert n' s ->
                if Set.size s' >= min' then pure s' else loop min' s'

-- | Select a node from the passive view at random.
--
-- If the passive view is empty, 'Nothing' is returned, otherwise 'Just' the
-- random node.
randomPassiveNode :: HyParView n c (Maybe n)
randomPassiveNode = do
    prng <- asks hPRNG
    pasv <- gets passive
    randomFromSet prng pasv

-- | Select a node @n'@ from the passive view at random, such that @n' /= n@
randomPassiveNodeNot :: Ord n => n -> HyParView n c (Maybe n)
randomPassiveNodeNot n = do
    prng <- asks hPRNG
    pasv <- gets passive
    randomFromSet prng (Set.delete n pasv)

-- | Select 'num' nodes from the passive view at random.
--
-- If the passive view is empty, the empty set is returned. 'num' is adjusted to
-- be:
--
-- > min (size passive) (min cfgMaxPassive num)
randomPassiveNodes :: Ord n => Int -> HyParView n c (Set n)
randomPassiveNodes num = do
    pasv <- gets passive
    Handle{hConfig, hPRNG} <- ask
    let min' = min (Set.size pasv) . min num . fromIntegral . cfgMaxPassive
             $ hConfig
     in loop min' pasv hPRNG Set.empty
  where
    loop min' pasv prng !s = do
        rpn <- randomFromSet prng pasv
        case rpn of
            Nothing -> pure s
            Just n  | s' <- Set.insert n s ->
                if Set.size s' >= min' then pure s' else loop min' pasv prng s'

isActiveAtCapacity :: HyParView n c Bool
isActiveAtCapacity = do
    maxa <- asks $ fromIntegral . cfgMaxActive . hConfig
    actv <- gets active
    pure $ Map.size actv >= maxa

isPassiveAtCapacity :: HyParView n c Bool
isPassiveAtCapacity = do
    maxp <- asks $ fromIntegral . cfgMaxPassive . hConfig
    pasv <- gets passive
    pure $ Set.size pasv >= maxp

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
addToActive :: Ord n => n -> HyParView n c (Maybe n)
addToActive n =
    asks hSelf >>= \self -> if self == n then pure Nothing else go
  where
    go = do
        -- Remove first, so we can't remove 'new' right away.
        removed <- removeFromActiveIfFull
        conn    <-
            openConnection n `onException`
                -- Put back removed if we can't connect.
                for_ removed (\(r, c) -> modify' $ over activeL (Map.insert r c))
        modify' $ over activeL (Map.insert n conn)
        notifyUp n

        -- Now close the connection of the removed node, if any.
        traverse_ (closeConnection . snd) removed

        -- If 'new' is in the passive view, remove it from there.
        pasv <- gets passive
        when (Set.member n pasv) $
            modify' $ over passiveL (Set.delete n)

        pure $ fst <$> removed

-- | Add a peer to the passive view, removing a random other node from the
-- passive view if it is full.
addToPassive :: Ord n => n -> HyParView n c ()
addToPassive n = do
    self <- asks hSelf
    actv <- gets active
    when (n /= self && Map.notMember n actv) $ do
        removeFromPassiveIfFull
        modify' $ over passiveL (Set.insert n)

purgePassive :: Ord n => Set n -> HyParView n c ()
purgePassive ns = modify' $ over passiveL (`Set.difference` ns)

removeFromActiveIfFull :: Ord n => HyParView n c (Maybe (n, c))
removeFromActiveIfFull = isActiveAtCapacity >>= bool (pure Nothing) go
  where
    go = do
        ran <- randomActiveNode
        rmd <-
            for ran $ \n -> do
                conn <- removeFromActive n
                pure $ (,) n <$> conn
        pure $ join rmd

removeFromActive :: Ord n => n -> HyParView n c (Maybe c)
removeFromActive n = do
    conn <-
        state $ \peers ->
            let conn   = Map.lookup n (active peers)
                peers' = over activeL (Map.delete n) peers
             in (conn, peers')
    traverse_ (const $ notifyDown n) conn
    pure conn

removeFromPassiveIfFull :: Ord n => HyParView n c (Maybe n)
removeFromPassiveIfFull = isPassiveAtCapacity >>= bool (pure Nothing) go
  where
    go = do
        rpn <- randomPassiveNode
        for_ rpn removeFromPassive
        pure rpn

removeFromPassive :: Ord n => n -> HyParView n c ()
removeFromPassive n = modify' $ over passiveL (Set.delete n)

notifyUp :: n -> HyParView n c ()
notifyUp n = do
    up <- asks $ neighborUp . hCallbacks
    io $ up n

notifyDown :: n -> HyParView n c ()
notifyDown n = do
    down <- asks $ neighborDown . hCallbacks
    io $ down n

addToActive' :: Ord n => n -> HyParView n c [RPC n]
addToActive' n =
    addToActive n >>= map maybeToList . traverse (`send` Disconnect)

send :: n -> Message n -> HyParView n c (RPC n)
send to payload = do
    s <- asks hSelf
    pure RPC
        { rpcSender    = s
        , rpcRecipient = to
        , rpcPayload   = payload
        }

openConnection :: n -> HyParView n c c
openConnection to = do
    open <- asks $ connOpen . hCallbacks
    io $ open to

closeConnection :: c -> HyParView n c ()
closeConnection conn = do
    close <- asks $ connClose . hCallbacks
    handleAny ignoreException $ io (close conn)

--------------------------------------------------------------------------------

ignoreException :: (Applicative m, Monoid w) => SomeException -> m w
ignoreException = const $ pure mempty

randomFromSet :: PrimMonad m => Gen (PrimState m) -> Set a -> m (Maybe a)
randomFromSet prng s
    | Set.null s = pure Nothing
    | otherwise  = pure . (`Set.elemAt` s) <$> uniformR (0, Set.size s - 1) prng

randomKeyFromMap :: PrimMonad m => Gen (PrimState m) -> Map k a -> m (Maybe k)
randomKeyFromMap prng m
    | Map.null m = pure Nothing
    | otherwise  = pure . fst . (`Map.elemAt` m) <$> uniformR (0, Map.size m - 1) prng
