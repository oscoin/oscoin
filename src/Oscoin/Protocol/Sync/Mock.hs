{-# LANGUAGE DataKinds #-}
module Oscoin.Protocol.Sync.Mock where

import           Oscoin.Crypto
import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain, blocks, tip)
import           Oscoin.Crypto.Blockchain.Block
                 (Block, BlockHeader, Score, Sealed, blockHeader, blockHeight)
import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Protocol.Sync
import qualified Oscoin.Storage.Block.Pure as BlockStore.Pure
import           Oscoin.Telemetry.Trace
import           Oscoin.Time.Chrono as Chrono

import           Control.Monad.RWS.Strict hiding (get, gets)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Lens.Micro (Lens', lens)


{------------------------------------------------------------------------------
  Instances
------------------------------------------------------------------------------}

type instance ProtocolResponse c MockTx MockSeal 'GetTip =
        Block c MockTx (Sealed c MockSeal)
type instance ProtocolResponse c MockTx MockSeal 'GetBlocks =
        NewestFirst [] (Block c MockTx (Sealed c MockSeal))
type instance ProtocolResponse c MockTx MockSeal 'GetBlockHeaders =
        NewestFirst [] (BlockHeader c (Sealed c MockSeal))

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
toMockPeers (WorldState m _) = HS.fromMap (() <$ m)

-- | A mock 'SyncContext' operating in a mock/simulated environment.
mockContext
    :: ( Eq (Crypto.PublicKey MockCrypto)
       , Hashable (Crypto.PublicKey MockCrypto)
       , Crypto.Hashable MockCrypto MockTx
       )
    => SyncContext MockCrypto MockTx MockSeal Sim
mockContext =
    SyncContext
        { scNu               = 0  -- can be adjusted later.
        , scActivePeers      = gets toMockPeers
        , scDataFetcher      = mkDataFetcherSim
        , scEventTracer      = probed noProbe
        , scConcurrently     = forM
        , scLocalChainReader = fst (BlockStore.Pure.mkStateBlockStore chainReaderL)
        , scUpstreamConsumers = [
          \se -> tell [se]
                                ]
        }


-- | Creates a new simulation 'DataFetcher'.
mkDataFetcherSim
    :: ( Eq (Crypto.PublicKey MockCrypto)
       , Hashable (Crypto.PublicKey MockCrypto)
       )
    => DataFetcher MockCrypto MockTx MockSeal Sim
mkDataFetcherSim = DataFetcher $ \peer -> \case
    SGetTip -> \() -> do
        WorldState{..} <- get
        case HM.lookup peer _mockPeerState of
            Nothing -> panic "newDataFetcherSim - precondition violation, empty blockchain."
            Just bc -> pure $ Right $ (tip bc)
    -- NOTE(adn) We are leaking our abstractions here and bypassing the
    -- 'BlockStoreReader' interface, but this is an interim measure while we
    -- wait for oscoin#550.
    SGetBlocks -> \Range{..} ->   Right
                                . NewestFirst
                                . take (fromIntegral $ end - start)
                                . drop (fromIntegral start)
                              <$> getAllBlocks
    -- NOTE(adn) We are leaking our abstractions here and bypassing the
    -- 'BlockStoreReader' interface, but this is an interim measure while we
    -- wait for oscoin#550.
    SGetBlockHeaders -> \Range{..} ->   Right
                                      . NewestFirst
                                      . take (fromIntegral $ end - start)
                                      . drop (fromIntegral start)
                                      . map blockHeader
                                    <$> getAllBlocks
  where
      getAllBlocks :: Sim [Block MockCrypto MockTx (Sealed MockCrypto MockSeal)]
      getAllBlocks = gets ( Chrono.toOldestFirst
                          . Chrono.reverse
                          . blocks
                          . BlockStore.Pure.getBestChain
                          . _mockLocalChain
                          )

{------------------------------------------------------------------------------
  Running a Sync operation
------------------------------------------------------------------------------}

-- | Runs a 'Sync' operation on the simulated environment.
runMockSync
    :: WorldState
    -> SyncContext MockCrypto MockTx MockSeal Sim
    -> Sync MockCrypto MockTx MockSeal Sim a
    -> (Either SyncError a, [SyncEvent MockCrypto MockTx MockSeal])
runMockSync ws syncContext (Sync s) =
    evalRWS (runReaderT (runExceptT s) $ syncContext) () ws
