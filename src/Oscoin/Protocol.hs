module Oscoin.Protocol
    ( Protocol -- opaque
    , Handle(..)
    , hoistHandle

    -- * Getters
    , protoOrphanage

    -- * Running and using the Protocol
    , withProtocol
    , runProtocol

    -- * Internals
    , stepProtocol
    ) where

import           Oscoin.Prelude

import           Formatting (Buildable)
import           Oscoin.Consensus (validateBlockchain)
import           Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Types (Validate)
import           Oscoin.Crypto.Blockchain (Blockchain(..))
import           Oscoin.Crypto.Blockchain.Block hiding (parentHash)
import qualified Oscoin.Crypto.Blockchain.Block as Block
import           Oscoin.Crypto.Hash (HasHashing, Hash)
import           Oscoin.Storage.Block.Abstract
                 (BlockStore, BlockStoreReader, BlockStoreWriter)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.Block.Orphanage (Orphanage)
import qualified Oscoin.Storage.Block.Orphanage as O
import           Oscoin.Telemetry (NotableEvent(..))
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Time.Chrono as Chrono

import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM
import qualified Data.List.NonEmpty as NonEmpty

-- | This data structure incorporates all the different components and
-- data structures in the system devoted to store \"things\" and enforce
-- the Oscoin protocol, which entails also chain selection and blocks
-- validation. Certain components (like the block cache, for example) are not
-- managed by the 'Protocol' directly but they are rather an implementation
-- detail of whichever 'BlockStore' this structure encapsulates.
data Protocol c tx s m = Protocol
    { protoFullBlockStore :: BlockStore c tx s m
    -- ^ The 'BlockStore', an opaque interface to a component which only
    -- responsibility is to store and retrieve blocks.
    , protoOrphanage      :: Orphanage c tx s
    -- ^ The in-memory data structure which keeps orphans blocks and constructs
    -- sub-chains suitable for fork selection.
    , protoValidateFull   :: Validate c tx s
    -- ^ A validation function to (fully) validate a block.
    , protoScoreBlock     :: Block c tx (Sealed c s) -> Score
    -- ^ A function to score blocks.
    , protoConfig         :: Consensus.Config
    }


-- | A 'Handle' is a record of functions containing two possible
-- \"strategies\" to dispatch block; calling `dispatchBlockSync` ensures the
-- function won't return until the `Protocol` has been fully stepped
-- (i.e. block inserted in the store, chain selection fully performed etc.)
-- `dispatchBlockAsync` as the name implies, doesn't block and returns the
-- control to the caller immediately, but offers less integrity guarantees.
data Handle c tx s m = Handle
    { dispatchBlockSync  :: Block c tx (Sealed c s) -> m ()
    -- ^ TODO(adn): Return an error?
    , dispatchBlockAsync :: Block c tx (Sealed c s) -> m ()
    , isNovelBlock       :: BlockHash c -> m Bool
    }

hoistHandle
    :: forall c tx s n m. (forall a. n a -> m a)
    -> Handle c tx s n
    -> Handle c tx s m
hoistHandle natTrans hdl = Handle
    { dispatchBlockSync  = natTrans . dispatchBlockSync hdl
    , dispatchBlockAsync = natTrans . dispatchBlockAsync hdl
    , isNovelBlock       = natTrans . isNovelBlock hdl
    }

{------------------------------------------------------------------------------
  Using the 'Protocol'
------------------------------------------------------------------------------}

-- | Given the data necessary to initialise and build the 'Protocol', runs
-- the input action and returns the final result.
withProtocol
    :: forall c tx s a m.
       Orphanage c tx s
    -> Validate c tx s
    -> (Block c tx (Sealed c s) -> Score)
    -> BlockStore c tx s m
    -> Consensus.Config
    -> (Protocol c tx s m -> m a)
    -> m a
withProtocol o validateFull scoreBlock bs config action =
    let protocol = Protocol bs o validateFull scoreBlock config
    in action protocol

-- | Initialises and runs the 'Protocol'.
-- Returns a 'Handle' which can be used to submit new blocks to the 'Protocol'
-- either in a blocking (cfr. 'dispatchBlockSync') or a non-blocking
-- (cfr. 'dispatchBlockAsync') fashion.
--
-- N.b. In order to dispatch blocks in a non-blocking way, this function
-- internally spawns an asynchronous worker as a green thread. As we do not
-- want such worker to go dead silently, we rethrow any exception raised by
-- the latter in the main thread.
runProtocol
    :: forall c tx s a.
       ( Ord s
       , HasHashing c
       , Buildable (Hash c)
       )
    => Validate c tx s
    -> (Block c tx (Sealed c s) -> Score)
    -- ^ A function to score a block.
    -> Telemetry.Handle
    -> BlockStore c tx s IO
    -> Consensus.Config
    -> (Handle c tx s IO -> IO a)
    -> IO a
runProtocol validateFull scoreBlock telemetry bs config use =
  bracket acquire dispose (\(hdl, _worker) -> use hdl)
  where
    acquire :: IO (Handle c tx s IO, Async ())
    acquire = do
        let o = O.emptyOrphanage scoreBlock
        proto               <- newMVar (Protocol bs o validateFull scoreBlock config)
        incomingBlocksQueue <- atomically $ newTBQueue 64

        worker <- Async.async $ forever $ do
                      block  <- atomically $ readTBQueue incomingBlocksQueue
                      events <- modifyMVar proto $ \p -> stepProtocol p block
                      forM_ events (Telemetry.emit telemetry)

        Async.link worker

        let hdl = Handle
              { dispatchBlockSync = \blk -> do
                  events <- modifyMVar proto $ \p -> stepProtocol p blk
                  forM_ events (Telemetry.emit telemetry)
              , dispatchBlockAsync = atomically . writeTBQueue incomingBlocksQueue
              , isNovelBlock = \h -> withMVar proto $ \p -> isNovelBlockInternal p h
              }

        pure (hdl, worker)

    -- TODO(adn) Log/emit that we are stopping the protocol runner.
    dispose :: (Handle c tx s IO, Async ()) -> IO ()
    dispose = Async.cancel . snd

-- | Main powerhouse of the 'Protocol' module. In order, this endless
-- loop does the following:
--
-- 1. Gets incoming blocks from the outside;
-- 2. Performs full (extrinsic) block validation;
-- 3. Stores validated blocks into the BlockStore or as orphans;
-- 4. Performs chain selection.
-- /NOTE/: This function doesn't perform basic validation, and it assumes
-- the incoming block has been already \"pre-validated\" at the gossip level.
stepProtocol
    :: forall c tx s m.
       ( Ord (BlockHash c)
       , Ord s
       , HasHashing c
       , Buildable (Hash c)
       , Monad m
       )
    => Protocol c tx s m
    -> Block c tx (Sealed c s)
    -> m (Protocol c tx s m, [NotableEvent])
stepProtocol mgr incomingBlock = do
    -- Step 2. Try to store the block; if this is recognised as to be an
    -- orphan, we store it immediately. If this block is scheduled for
    -- inclusion to extend the tip of the chain, then its full validity is
    -- enforced.
    extendsTip <- do
        currentTip <- BlockStore.getTip bsPublicAPI
        pure $ blockHash currentTip == Block.parentHash incomingBlock

    (mgr', evts) <-
       if extendsTip
          then do
              -- Get the last (cached) 'mutableChainDepth' blocks and performs
              -- full block validation.
              let depth = fromIntegral $ mutableChainDepth (protoConfig mgr)
              ancestors <- BlockStore.getBlocksByDepth bsPublicAPI depth

              case protoValidateFull mgr (toNewestFirst ancestors) incomingBlock of
                  Left validationError ->
                      let evt = BlockValidationFailedEvent (blockHash incomingBlock) validationError
                      in pure (mgr, [evt])
                  Right () -> do
                      -- Step 3: store the fully-validated block.
                      BlockStore.insertBlock bsPrivateAPI incomingBlock
                      pure (mgr, mempty)

          -- NOTE(adn) This is a side effect of the fact that storing
          -- the genesis as an orphan would insert it into the orphanage with
          -- its parent hash, which means that we won't be able to retrieve
          -- a potential candidate branching off from genesis in our 'selectBestChain'
          -- function (cfr. QuickCheck seed 554786). To avoid that, we avoid
          -- inserting the genesis block into the orphanage in the first place;
          -- After all, the genesis block should never be considered an orphan.
          else map (,mempty) . pure $
              if not (isGenesisBlock incomingBlock)
                  then mgr { protoOrphanage = O.insertOrphan incomingBlock (protoOrphanage mgr) }
                  else mgr

    -- Step 4: Chain selection.
    (,evts) <$> selectBestChain mgr'
  where
      bsPublicAPI :: BlockStoreReader c tx s m
      bsPublicAPI = fst . protoFullBlockStore $ mgr

      bsPrivateAPI :: BlockStoreWriter c tx s m
      bsPrivateAPI = snd . protoFullBlockStore $ mgr

{------------------------------------------------------------------------------
  (Best) Chain selection
------------------------------------------------------------------------------}

-- | Performs best chain selection and returns the new 'Protocol' data
-- structure.
selectBestChain
    :: forall m s c tx.
       ( Monad m
       , Ord s
       , Ord (BlockHash c)
       )
    => Protocol c tx s m
    -> m (Protocol c tx s m)
selectBestChain mgr = do
    -- NOTE(adn) Grab the \"mutable\" part of the chain and
    -- compare each root hash to see if there is any fork originating from that.
    let depth = fromIntegral $ mutableChainDepth (protoConfig mgr)
    mutableBlockHashes <-
        map blockHash . Chrono.reverse <$> BlockStore.getBlocksByDepth bsPublicAPI depth

    case O.selectBestChain (toOldestFirst mutableBlockHashes) orphanage of
        Just (parentHash, bestFork) | isValid bestFork -> do
            -- Compare the orphan best chain with the chain suffix currently adopted
            chainSuffix <- BlockStore.getBlocksByParentHash bsPublicAPI parentHash

            -- Switch-to-better-chain condition: either the chain suffix is
            -- empty, which means we are extending directly the tip, or if we
            -- found a better candidate.
            let scoreFn       = protoScoreBlock mgr
                newChainFound =
                    null chainSuffix
                 || bestFork > O.fromChainSuffix scoreFn (OldestFirst . NonEmpty.reverse
                                                                      . NonEmpty.fromList
                                                                      . toNewestFirst $ chainSuffix
                                                         )

            if newChainFound
               then do
                   switchToFork bestFork chainSuffix
                   -- Prune the chain which won, /as well as/ every dangling orphan
                   -- originating from the suffix chain which has been replaced.
                   let orphanage' =
                         foldl' (\acc blockInSuffix ->
                                    O.pruneOrphanage (blockHash blockInSuffix)
                                                     (O.fromChainSuffix scoreFn (OldestFirst $ blockInSuffix NonEmpty.:| []))
                                                     $ acc
                                ) (O.pruneOrphanage parentHash bestFork orphanage) chainSuffix
                   pure $ mgr { protoOrphanage = orphanage' }
               else pure mgr
        _ -> pure mgr

  where

    isValid :: O.ChainCandidate c s -> Bool
    isValid candidate =
        let chain = Blockchain
                  . toNewestFirst
                  . Chrono.reverse
                  . O.toBlocksOldestFirst orphanage
                  $ candidate
        in case validateBlockchain validateFull chain of
             Left _   -> False
             Right () -> True

    bsPublicAPI :: BlockStoreReader c tx s m
    bsPublicAPI = fst . protoFullBlockStore $ mgr

    bsPrivateAPI :: BlockStoreWriter c tx s m
    bsPrivateAPI = snd . protoFullBlockStore $ mgr

    validateFull = protoValidateFull mgr

    orphanage  = protoOrphanage mgr

    switchToFork fork chainSuffix = do
        let newChain = O.toBlocksOldestFirst orphanage fork
        BlockStore.switchToFork bsPrivateAPI (fromIntegral $ length chainSuffix) newChain

-- | Returns 'True' if the input block is novel to the 'Protocol', i.e it's
-- neither in the block store nor in the orphanage.
isNovelBlockInternal
    :: (Ord (BlockHash c), Monad m)
    => Protocol c tx s m
    -> BlockHash c
    -> m Bool
isNovelBlockInternal proto h = do
    insideBlockStore <- not <$> BlockStore.isNovelBlock bsPublicAPI h
    pure (not insideBlockStore && not (O.member orphanage h))
  where
    orphanage   = protoOrphanage proto
    bsPublicAPI = fst . protoFullBlockStore $ proto
