{-# LANGUAGE DataKinds #-}
module Oscoin.Protocol.Sync.Mock where

import           Oscoin.Crypto
import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain, tip)
import           Oscoin.Crypto.Blockchain.Block
                 (Block, Score, Sealed, blockHeader, blockHeight)
import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Protocol.Sync
import qualified Oscoin.Storage.Block.Abstract as BlockStore
import qualified Oscoin.Storage.Block.Pure as BlockStore.Pure
import           Oscoin.Telemetry.Trace
import           Oscoin.Time.Chrono as Chrono

import           Control.Monad.RWS.Strict hiding (get, gets)
import           Data.Conduit.Combinators (yieldMany)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import           Lens.Micro (Lens', lens)


{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type MockTx = Word8

type MockSeal = Text

type MockPeer = ActivePeer MockCrypto

type PeerData = Blockchain MockCrypto MockTx MockSeal

data WorldState = WorldState
    { _mockPeerState  :: HashMap MockPeer PeerData
    , _mockLocalChain :: BlockStore.Pure.Handle MockCrypto MockTx MockSeal

    }

type Sim = RWS () [SyncEvent MockCrypto MockTx MockSeal] WorldState

{------------------------------------------------------------------------------
  Smart constructors / helper functions / combinators
------------------------------------------------------------------------------}

mockPeers :: Lens' WorldState (HashMap MockPeer PeerData)
mockPeers = lens _mockPeerState (\s a -> s { _mockPeerState = a })

chainReaderL :: Lens' WorldState    (BlockStore.Pure.Handle MockCrypto MockTx MockSeal)
chainReaderL = lens _mockLocalChain (\s a -> s { _mockLocalChain = a })


-- Score blocks by height.
mockScore :: Block c tx (Sealed c s) -> Score
mockScore = blockHeight . blockHeader

emptyWorldState
    :: ( Eq (Crypto.PublicKey MockCrypto)
       , Ord (Hash MockCrypto)
       , Hashable (Crypto.PublicKey MockCrypto)
       )
    => Block MockCrypto MockTx (Sealed MockCrypto MockSeal)
    -- ^ The genesis block.
    -> WorldState
emptyWorldState genesisBlock =
    WorldState mempty (BlockStore.Pure.genesisBlockStore genesisBlock mockScore)

toMockPeers :: WorldState -> HashSet MockPeer
toMockPeers (WorldState m _) = Set.fromMap (() <$ m)

-- | A mock 'SyncContext' operating in a mock/simulated environment.
mockContext
    :: ( Eq (Crypto.PublicKey MockCrypto)
       , Hashable (Crypto.PublicKey MockCrypto)
       , Crypto.Hashable MockCrypto MockTx
       )
    => SyncContext MockCrypto MockTx MockSeal Sim
mockContext =
    SyncContext
        { scNu                = 0  -- can be adjusted later.
        , scActivePeers       = gets toMockPeers
        , scDataFetcher       = mkDataFetcherSim
        , scEventTracer       = probed noProbe
        , scConcurrently      = forM
        , scLocalChainReader  = fst (BlockStore.Pure.mkStateBlockStore chainReaderL)
        , scEventHandlers = [ \se -> tell [se] ]
        }

-- | Creates a new simulation 'DataFetcher'.
mkDataFetcherSim
    :: ( Eq (Crypto.PublicKey MockCrypto)
       , Hashable (Crypto.PublicKey MockCrypto)
       , Crypto.Hashable MockCrypto MockTx
       )
    => DataFetcher MockCrypto MockTx MockSeal Sim
mkDataFetcherSim = DataFetcher $ \peer -> \case
    SGetTip -> \() -> do
        WorldState{..} <- get
        case HM.lookup peer _mockPeerState of
            Nothing -> panic "newDataFetcherSim - precondition violation, empty blockchain."
            Just bc -> pure . Right $ tip bc

    SGetBlocks -> \rng -> do
        blocks <- lookupBlocks rng peer
        pure $ Right (yieldMany blocks)

    SGetBlockHeaders -> \rng -> do
        blocks <- lookupBlocks rng peer
        pure $ Right (yieldMany (map blockHeader blocks))
  where
      lookupBlocks
          :: Range
          -> MockPeer
          -> Sim [Block MockCrypto MockTx (Sealed MockCrypto MockSeal)]
      lookupBlocks Range{..} peer = do
          WorldState{..} <- get
          case HM.lookup peer _mockPeerState of
            Nothing -> pure []
            Just c  -> do
              -- The peers are read-only and don't do chain selection, so
              -- the score function is irrelevant here.
              let peerChain = BlockStore.Pure.initWithChain c mockScore
              let store = fst (BlockStore.Pure.mkStateBlockStore (chainL peerChain))
              Chrono.toOldestFirst <$> BlockStore.lookupBlocksByHeight store (rangeStart, rangeEnd)

      chainL
          :: BlockStore.Pure.Handle MockCrypto MockTx MockSeal
          -> Lens' WorldState (BlockStore.Pure.Handle MockCrypto MockTx MockSeal)
      chainL peerChain = lens (const peerChain) const

{------------------------------------------------------------------------------
  Running a Sync operation
------------------------------------------------------------------------------}

-- | Runs a 'Sync' operation on the simulated environment.
runMockSync
    :: WorldState
    -> SyncContext MockCrypto MockTx MockSeal Sim
    -> SyncT MockCrypto MockTx MockSeal Sim a
    -> (Either SyncError a, [SyncEvent MockCrypto MockTx MockSeal])
runMockSync ws syncContext (SyncT s) =
    evalRWS (runReaderT (runExceptT s) $ syncContext) () ws
