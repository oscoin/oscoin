module Network.Gossip.Plumtree.Scheduler
    ( Handle
    , SchedulerT
    , LazyFlushInterval

    , new

    , withScheduler
    , runSchedulerT

    , sendLazy
    , later
    , cancel
    ) where

import           Prelude

import qualified Network.Gossip.Plumtree as P

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (atomically)
import           Control.Exception.Safe
import           Control.Monad (forever)
import           Control.Monad.Reader
import           Data.Foldable (traverse_)
import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Data.IORef (IORef, mkWeakIORef, newIORef)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Time.Clock (NominalDiffTime)
import           Data.Unique
import           Data.Void (Void)
import qualified Focus
import qualified ListT
import qualified STMContainers.Map as STMMap

data Handle n = Handle
    { hFlushInterval :: LazyFlushInterval
    , hLazyQueue     :: STMMap.Map n (HashSet (P.IHave n))
    , hDeferred      :: STMMap.Map P.MessageId (Map Unique (Async ()))
    , _alive         :: IORef ()
    }

type LazyFlushInterval = NominalDiffTime

new :: LazyFlushInterval -> IO (Handle n)
new hFlushInterval = do
    hLazyQueue <- STMMap.newIO
    hDeferred  <- STMMap.newIO
    _alive     <- newIORef ()
    let hdl = Handle {..}

    void $ mkWeakIORef _alive (destroy hdl)
    pure hdl

destroy :: Handle n -> IO ()
destroy Handle { hDeferred } = do
    deferreds <-
        atomically
            . ListT.fold
                (\xs -> pure . (xs <>) . map Async.uninterruptibleCancel . Map.elems . snd)
                []
            $ STMMap.stream hDeferred
    sequence_ deferreds

withScheduler :: Handle n -> (n -> HashSet (P.IHave n) -> IO ()) -> IO a -> IO a
withScheduler hdl send k =
    bracket (runFlusher hdl send) shutdown (const k)
  where
    -- TODO(kim): allow draining before exit
    shutdown flush =
        Async.uninterruptibleCancel flush `finally` destroy hdl

newtype SchedulerT n m a = SchedulerT (ReaderT (Handle n) m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader (Handle n)
             )

runSchedulerT :: Handle n -> SchedulerT n m a -> m a
runSchedulerT r (SchedulerT ma) = runReaderT ma r

sendLazy :: (Eq n, Hashable n, MonadIO m) => n -> P.IHave n -> SchedulerT n m ()
sendLazy to ihave = do
    queue <- asks hLazyQueue
    liftIO . atomically $ STMMap.focus upsert to queue
  where
    upsert = Focus.alterM $
        pure . Just . maybe (Set.singleton ihave) (Set.insert ihave)

later :: MonadIO m => NominalDiffTime -> P.MessageId -> IO () -> SchedulerT n m ()
later timeout mid action = do
    defs <- asks hDeferred
    liftIO $ do
        uniq    <- newUnique
        action' <-
            async $ do
                sleep timeout
                action
                    `finally` atomically (STMMap.focus (expunge uniq) mid defs)
        atomically $ STMMap.focus (upsert uniq action') mid defs
  where
    upsert uniq act = Focus.alterM $
        pure . Just . maybe (Map.singleton uniq act) (Map.insert uniq act)

    expunge _    Nothing     = pure ((), Focus.Remove)
    expunge uniq (Just acts) = do
        let acts' = Map.delete uniq acts
        if Map.null acts' then
            pure ((), Focus.Remove)
        else
            pure ((), Focus.Replace acts')

cancel :: MonadIO m => P.MessageId -> SchedulerT n m ()
cancel mid = do
    defs <- asks hDeferred
    liftIO $ do
        actions <-
            atomically $
                STMMap.focus (pure . (,Focus.Remove)) mid defs
        (traverse_ . traverse_) Async.cancel actions

-- Internal --------------------------------------------------------------------

runFlusher :: Handle n -> (n -> HashSet (P.IHave n) -> IO ()) -> IO (Async Void)
runFlusher hdl send = async . forever $ do
    sleep $ hFlushInterval hdl
    ihaves <-
        atomically $ do
            ihaves <- ListT.toList $ STMMap.stream (hLazyQueue hdl)
            ihaves `seq` STMMap.deleteAll (hLazyQueue hdl)
            pure ihaves
    traverse_ (uncurry send) ihaves

sleep :: NominalDiffTime -> IO ()
sleep t = threadDelay $ toSeconds t * 1000000
  where
    toSeconds = round @Double . realToFrac
