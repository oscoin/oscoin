module Oscoin.Storage.Block.BlockTree.RealWorld
    ( newBlockTree
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus (validateBlockchain)
import           Oscoin.Consensus.Config as Consensus
import           Oscoin.Consensus.Types (Validate)
import           Oscoin.Crypto.Blockchain hiding (parentHash)
import           Oscoin.Crypto.Hash (Hash)
import           Oscoin.Storage.Block.Abstract
                 (BlockStore, BlockStoreReader, BlockStoreWriter)
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import           Oscoin.Storage.Block.BlockTree
import           Oscoin.Storage.Block.Orphanage (Orphanage)
import qualified Oscoin.Storage.Block.Orphanage as O
import           Oscoin.Time.Chrono as Chrono

import qualified Data.List.NonEmpty as NonEmpty

newBlockTree
    :: ( Monad m
       , Ord s
       , Ord (Hash c)
       )
    => Consensus.Config
    -> Validate c tx s
    -> (Block c tx (Sealed c s) -> Score)
    -> BlockStore c tx s m
    -> BlockTree c tx s m
newBlockTree cfg validateFull scoreBlock =
    newBlockTreeInternal (O.emptyOrphanage scoreBlock) cfg validateFull scoreBlock

newBlockTreeInternal
    :: ( Monad m
       , Ord s
       , Ord (Hash c)
       )
    => Orphanage c tx s
    -> Consensus.Config
    -> Validate c tx s
    -> (Block c tx (Sealed c s) -> Score)
    -> BlockStore c tx s m
    -> BlockTree c tx s m
newBlockTreeInternal o cfg validateFull scoreBlock bs =
    let btree = BlockTree
            { btFullBlockStore = bs
            , insertOrphan = \blk -> do
                o' <- chainSelection (O.insertOrphan blk o) cfg validateFull scoreBlock bs
                pure $ newBlockTreeInternal o' cfg validateFull scoreBlock bs
            , member = \h -> do
                  insideBlockStore <- BlockStore.member (fst bs) h
                  pure (insideBlockStore || O.member o h)
            }
    in btree

-- | Performs best chain selection and returns the updated 'BlockTree'.
-- structure.
chainSelection
    :: forall m s c tx.
       ( Monad m
       , Ord s
       , Ord (BlockHash c)
       )
    => O.Orphanage c tx s
    -> Consensus.Config
    -> Validate c tx s
    -> (Block c tx (Sealed c s) -> Score)
    -> BlockStore c tx s m
    -> m (Orphanage c tx s)
chainSelection orphanage cfg validateFull scoreBlock bs = do
    -- NOTE(adn) Grab the \"mutable\" part of the chain and
    -- compare each root hash to see if there is any fork originating from that.
    let depth = fromIntegral $ mutableChainDepth cfg
    mutableBlockHashes <-
        map blockHash . Chrono.reverse <$> BlockStore.getBlocksByDepth bsPublicAPI depth

    case O.selectBestChain (toOldestFirst mutableBlockHashes) orphanage of
        Just (parentHash, bestFork) | isValid bestFork -> do
            -- Compare the orphan best chain with the chain suffix currently adopted
            chainSuffix <- BlockStore.getBlocksByParentHash bsPublicAPI parentHash

            -- Switch-to-better-chain condition: either the chain suffix is
            -- empty, which means we are extending directly the tip, or if we
            -- found a better candidate.
            let newChainFound =
                    null chainSuffix
                 || bestFork > O.fromChainSuffix scoreBlock (OldestFirst . NonEmpty.reverse
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
                                                     (O.fromChainSuffix scoreBlock (OldestFirst $ blockInSuffix NonEmpty.:| []))
                                                     $ acc
                                ) (O.pruneOrphanage parentHash bestFork orphanage) chainSuffix
                   pure orphanage'
               else pure orphanage
        _ -> pure orphanage

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
    bsPublicAPI = fst bs

    bsPrivateAPI :: BlockStoreWriter c tx s m
    bsPrivateAPI = snd bs

    switchToFork fork chainSuffix = do
        let newChain = O.toBlocksOldestFirst orphanage fork
        BlockStore.switchToFork bsPrivateAPI (fromIntegral $ length chainSuffix) newChain
