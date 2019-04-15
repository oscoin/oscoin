module Oscoin.Protocol
    ( Protocol -- opaque
    , Handle(..)
    , hoistHandle

    -- * Running and using the Protocol
    , withProtocol
    , runProtocol

    -- * Internals
    , stepProtocol
    ) where

import           Oscoin.Prelude

import           Formatting (Buildable)
import           Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Types (Validate)
import           Oscoin.Crypto.Blockchain.Block hiding (parentHash)
import qualified Oscoin.Crypto.Blockchain.Block as Block
import           Oscoin.Crypto.Hash (HasHashing, Hash)
import           Oscoin.Protocol.Internal (Protocol(..))
import           Oscoin.Storage.Block.Abstract
                 (BlockStoreReader, BlockStoreWriter)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.Block.BlockTree (BlockTree(btFullBlockStore))
import qualified Oscoin.Storage.Block.BlockTree as BlockTree
import           Oscoin.Telemetry (NotableEvent(..))
import qualified Oscoin.Telemetry as Telemetry
import           Oscoin.Time.Chrono as Chrono

import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM

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
       Validate c tx s
    -> (Block c tx (Sealed c s) -> Score)
    -> BlockTree c tx s m
    -> Consensus.Config
    -> (Protocol c tx s m -> m a)
    -> m a
withProtocol validateFull scoreBlock btree config action =
    let protocol = Protocol btree validateFull scoreBlock config
    in action protocol

-- | Initialises and runs the 'Protocol', by spinning an asynchronous worker
-- which executes the 'fetchNextBlock' function to pull new blocks from outside
-- and steps the protocol.
runProtocol
    :: forall c tx s a.
       ( HasHashing c
       , Buildable (Hash c)
       )
    => Validate c tx s
    -> (Block c tx (Sealed c s) -> Score)
    -- ^ A function to score a block.
    -> Telemetry.Handle
    -> BlockTree c tx s IO
    -> Consensus.Config
    -> (Handle c tx s IO -> IO a)
    -> IO a
runProtocol validateFull scoreBlock telemetry btree config use =
  bracket acquire dispose (\(hdl, _worker) -> use hdl)
  where
    acquire :: IO (Handle c tx s IO, Async ())
    acquire = do
        proto               <- newMVar (Protocol btree validateFull scoreBlock config)
        incomingBlocksQueue <- atomically $ newTBQueue 64
        let hdl = Handle
              { dispatchBlockSync = \blk -> do
                  events <- modifyMVar proto $ \p -> stepProtocol p blk
                  forM_ events (Telemetry.emit telemetry)
              , dispatchBlockAsync = atomically . writeTBQueue incomingBlocksQueue
              , isNovelBlock = \h -> withMVar proto $ \p -> BlockTree.isNovelBlock (protoBlockTree p) h
              }

        worker <- Async.async $ do
                      block  <- atomically $ readTBQueue incomingBlocksQueue
                      events <- modifyMVar proto $ \p -> stepProtocol p block
                      forM_ events (Telemetry.emit telemetry)
        pure (hdl, worker)

    dispose :: (Handle c tx s IO, Async ()) -> IO ()
    dispose (_, worker) =
        -- TODO(adn) Log/emit that we are stopping the protocol runner.
        Async.cancel worker

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
       , HasHashing c
       , Buildable (Hash c)
       , Monad m
       )
    => Protocol c tx s m
    -> Block c tx (Sealed c s)
    -> m (Protocol c tx s m, [NotableEvent])
stepProtocol proto incomingBlock = do
    -- Step 2. Try to store the block; if this is recognised as to be an
    -- orphan, we store it immediately. If this block is scheduled for
    -- inclusion to extend the tip of the chain, then its full validity is
    -- enforced.
    extendsTip <- do
        currentTip <- BlockStore.getTip bsPublicAPI
        pure $ blockHash currentTip == Block.parentHash incomingBlock

    (proto', evts) <-
       if extendsTip
          then do
              -- Get the last (cached) 'mutableChainDepth' blocks and performs
              -- full block validation.
              let depth = fromIntegral $ mutableChainDepth (protoConfig proto)
              ancestors <- BlockStore.getBlocksByDepth bsPublicAPI depth
              case protoValidateFull proto (toNewestFirst ancestors) incomingBlock of
                  Left validationError ->
                      let evt = BlockValidationFailedEvent (blockHash incomingBlock) validationError
                      in pure (proto, [evt])
                  Right () -> do
                      -- Step 3: store the fully-validated block.
                      BlockStore.insertBlock bsPrivateAPI incomingBlock
                      pure (proto, mempty)

          -- NOTE(adn) This is a side effect of the fact that storing
          -- the genesis as an orphan would insert it into the orphanage with
          -- its parent hash, which means that we won't be able to retrieve
          -- a potential candidate branching off from genesis in our 'selectBestChain'
          -- function (cfr. QuickCheck seed 554786). To avoid that, we avoid
          -- inserting the genesis block into the orphanage in the first place;
          -- After all, the genesis block should never be considered an orphan.
          else map (,mempty) $
              if not (isGenesisBlock incomingBlock)
                  then do
                      -- Step 4: insert the orphan and perform chain selection.
                      bt' <- BlockTree.insertOrphan (protoBlockTree proto) incomingBlock
                      pure $ proto { protoBlockTree = bt' }
                  else pure proto

    -- Step 4: Chain selection.
    pure (proto', evts)
  where
      bsPublicAPI :: BlockStoreReader c tx s m
      bsPublicAPI = fst . btFullBlockStore . protoBlockTree $ proto

      bsPrivateAPI :: BlockStoreWriter c tx s m
      bsPrivateAPI = snd . btFullBlockStore . protoBlockTree $ proto

