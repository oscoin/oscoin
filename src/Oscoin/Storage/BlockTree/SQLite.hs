module Oscoin.Storage.BlockTree.SQLite
    ( withBlockTree
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (validateBlockchain)
import           Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Types (Validate)
import           Oscoin.Crypto.Blockchain (Blockchain(..))
import           Oscoin.Crypto.Blockchain.Block
                 (Block(..), Depth, ScoreFn, Sealed, isGenesisBlock)
import qualified Oscoin.Crypto.Blockchain.Block as Block
import qualified Oscoin.Crypto.Hash as Crypto
import           Oscoin.Telemetry.Events (NotableEvent(..))
import           Oscoin.Time.Chrono as Chrono

import           Oscoin.Storage.Block.SQLite.Internal (Handle)
import qualified Oscoin.Storage.Block.SQLite.Internal as SQLite
import qualified Oscoin.Storage.BlockTree.Internal as Abstract

import           Oscoin.Storage.Block.Orphanage (Orphanage)
import qualified Oscoin.Storage.Block.Orphanage as O

import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField (ToField)

import           Codec.Serialise (Serialise)
import qualified Data.List.NonEmpty as NonEmpty
import           Formatting

-- | Bracket-style initialisation of the SQLite BlockTree
-- FIXME(adn): This internally calls 'initialize', which is obviously not
-- ideal when we have state on disk and we are re-opening a database rather
-- than starting from scratch. In practice, /for now/, we should be OK as
-- 'initialize' should be idempotent, but nevertheless we need a better
-- migration strategy/initialisation here.
withBlockTree
    :: forall c tx s b.
       ( ToField s
       , FromField s
       , FromField (Crypto.Hash c)
       , Serialise s
       , SQLite.StorableTx c tx
       , Buildable (Crypto.Hash c)
       , Eq s
       )
    => Consensus.Config
    -> Validate c tx s
    -- ^ A function to validate a block (or a chain of blocks)
    -> ScoreFn c tx (Sealed c s)
    -- ^ A function to score a block.
    -> String
    -- ^ The path where the DB will live on disk
    -> Block c tx (Sealed c s)
    -- ^ The genesis block (used to initialise the store)
    -> (Abstract.BlockTree c tx s IO -> IO b)
    -- ^ Action which uses the 'BlockTree'.
    -> IO b
withBlockTree config validateFull scoreBlock path genesisBlock action =
    bracket acquire release use
  where
      acquire :: IO (MVar (Orphanage c tx s), Handle c tx s)
      acquire = (,) <$> newMVar (O.emptyOrphanage scoreBlock)
                    <*> (SQLite.initialize genesisBlock =<< SQLite.open path)

      release :: (MVar (Orphanage c tx s), Handle c tx s) -> IO ()
      release (_, hdl) = SQLite.close hdl

      use :: (MVar (Orphanage c tx s), Handle c tx s)
          -> IO b
      use (orphansVar, internalHandle) =
          let newBlockTree =
                ( Abstract.BlockTreeReader {
                      Abstract.getGenesisBlock       = SQLite.getGenesisBlock internalHandle
                    , Abstract.lookupBlock           =
                        -- NOTE(adn): This won't lookup orphans (currently). Should we
                        -- do that? What are the implications from a point of view of the
                        -- caller's contract? i.e. will the caller expect the 'BlockTreeReader'
                        -- to only consider the main, adopted chain?
                        SQLite.lookupBlock internalHandle
                    , Abstract.lookupTx              =
                        -- NOTE(adn) Same considerations as per 'lookupBlock'.
                        SQLite.lookupTx internalHandle
                    , Abstract.getBlocksByDepth      = SQLite.getBlocks internalHandle
                    , Abstract.getBlocksByParentHash = SQLite.getChainSuffix internalHandle
                    , Abstract.getTip                = SQLite.getTip internalHandle
                    }
                , Abstract.BlockTreeWriter {
                      Abstract.insertBlock           =
                          insertBlock (Consensus.mutableChainDepth config)
                                      validateFull
                                      scoreBlock
                                      orphansVar
                                      internalHandle
                })
          in action newBlockTree


-- | FIXME(adn) There is a MVar-lock-hell going on here between the locking
-- imposed by the orphanage and the (artificial) one we added to the 'Handle'
-- to mitigate the concurrency bug.
insertBlock
    :: ( ToField s
       , FromField s
       , FromField (Crypto.Hash c)
       , Serialise s
       , SQLite.StorableTx c tx
       , Buildable (Crypto.Hash c)
       , Eq s
       )
    => Depth
    -> Validate c tx s
    -> ScoreFn c tx (Sealed c s)
    -> MVar (Orphanage c tx s)
    -> SQLite.Handle c tx s
    -> Block c tx (Sealed c s)
    -> IO (OldestFirst [] NotableEvent)
insertBlock mutableChainDepth validateFull scoreBlock orphanVar handle incomingBlock =
    modifyMVar orphanVar $ \orphanage -> do
        extendsTip <- do
            currentTip <- SQLite.getTip handle
            pure $ blockHash currentTip == Block.parentHash incomingBlock

        if extendsTip
           then do
               -- Get the last (cached) 'mutableChainDepth' blocks and performs
               -- full block validation.
               ancestors <- SQLite.getBlocks handle mutableChainDepth
               case validateFull (toNewestFirst ancestors) incomingBlock of
                   Left validationError ->
                       let evt = BlockValidationFailedEvent (blockHash incomingBlock) validationError
                       in pure (orphanage, OldestFirst [evt])
                   Right () -> do
                       -- Step 3: store the fully-validated block.
                       SQLite.storeBlock handle incomingBlock
                       pure (orphanage, OldestFirst [BlockchainTipExtended (blockHash incomingBlock)])

           -- NOTE(adn) This is a side effect of the fact that storing
           -- the genesis as an orphan would insert it into the orphanage with
           -- its parent hash, which means that we won't be able to retrieve
           -- a potential candidate branching off from genesis in our 'selectBestChain'
           -- function (cfr. QuickCheck seed 554786). To avoid that, we avoid
           -- inserting the genesis block into the orphanage in the first place;
           -- After all, the genesis block should never be considered an orphan.
           else if isGenesisBlock incomingBlock
               then pure (orphanage, mempty)
               else selectBestChain mutableChainDepth
                                    validateFull
                                    scoreBlock
                                    (O.insertOrphan incomingBlock orphanage)
                                    handle

selectBestChain
    :: forall c tx s.
       ( ToField s
       , FromField s
       , FromField (Crypto.Hash c)
       , Serialise s
       , SQLite.StorableTx c tx
       , Buildable (Crypto.Hash c)
       , Eq s
       )
    => Depth
    -> Validate c tx s
    -> ScoreFn c tx (Sealed c s)
    -> Orphanage c tx s
    -> Handle c tx s
    -> IO (Orphanage c tx s, OldestFirst [] NotableEvent)
selectBestChain mutableChainDepth validateFull scoreFn orphanage handle = do
    -- NOTE(adn) Grab the \"mutable\" part of the chain and
    -- compare each root hash to see if there is any fork originating from that.
    mutableBlockHashes <-
        map blockHash . Chrono.reverse <$> SQLite.getBlocks handle mutableChainDepth

    case O.selectBestChain (toOldestFirst mutableBlockHashes) orphanage of
        Just (parentHash, bestFork) | isValid bestFork -> do
            -- Compare the orphan best chain with the chain suffix currently adopted
            chainSuffix <- SQLite.getChainSuffix handle parentHash

            -- Switch-to-better-chain condition: either the chain suffix is
            -- empty, which means we are extending directly the tip, or if we
            -- found a better candidate.
            let newChainFound =
                    null chainSuffix
                 || bestFork > O.fromChainSuffix scoreFn (OldestFirst . NonEmpty.reverse
                                                                      . NonEmpty.fromList
                                                                      . toNewestFirst $ chainSuffix
                                                         )

            if newChainFound
               then do
                   rollbackEvents <- switchToFork bestFork chainSuffix
                   -- Prune the chain which won, /as well as/ every dangling orphan
                   -- originating from the suffix chain which has been replaced.
                   let orphanage' =
                         foldl' (\acc blockInSuffix ->
                                    O.pruneOrphanage (blockHash blockInSuffix)
                                                     (O.fromChainSuffix scoreFn (OldestFirst $ blockInSuffix NonEmpty.:| []))
                                                     $ acc
                                ) (O.pruneOrphanage parentHash bestFork orphanage) chainSuffix
                   pure $ (orphanage', rollbackEvents)
               else pure (orphanage, mempty)
        _ -> pure (orphanage, mempty)

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

    -- @switchToFork n bs@ removes the @n@ latest blocks and calls
    -- 'insertBlock' for each block in @bs@ starting from the first
    -- (i.e. the oldest) in the list.
    switchToFork
        :: O.ChainCandidate c s
        -> NewestFirst [] (Block c tx (Sealed c s))
        -> IO (OldestFirst [] NotableEvent)
    switchToFork fork chainSuffix = do
        let newChain = O.toBlocksOldestFirst orphanage fork
        SQLite.switchToFork handle (fromIntegral $ length chainSuffix) newChain
        pure $ OldestFirst $
            RollbackOccurred (fromIntegral $ length chainSuffix)
            : map (BlockchainTipExtended . blockHash) (NonEmpty.toList $ toOldestFirst newChain)

