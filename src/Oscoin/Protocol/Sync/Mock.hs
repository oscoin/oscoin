module Oscoin.Protocol.Sync.Mock where

import           Oscoin.Crypto
import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain (Blockchain, tip)
import           Oscoin.Crypto.Blockchain.Block (BlockHash)
import qualified Oscoin.Crypto.PubKey as Crypto
import           Oscoin.Protocol.Sync
import           Oscoin.Telemetry.Trace

import           Codec.Serialise as CBOR
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Lens.Micro (Lens', lens)

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

type MockPeer = ActivePeer MockCrypto

type PeerData = Blockchain MockCrypto [Word8] Text

data WorldState = WorldState
    { _mockPeerState :: HashMap MockPeer PeerData
    }

type Sim = StateT WorldState Identity

{------------------------------------------------------------------------------
  Smart constructors / helper functions / combinators
------------------------------------------------------------------------------}

mockPeers :: Lens' WorldState (HashMap MockPeer PeerData)
mockPeers = lens _mockPeerState (\s a -> s { _mockPeerState = a })
{-# INLINE mockPeers #-}

emptyWorldState
    :: ( Eq (Crypto.PublicKey MockCrypto)
       , Hashable (Crypto.PublicKey MockCrypto)
       )
    => WorldState
emptyWorldState = WorldState mempty

toMockPeers :: WorldState -> HashSet MockPeer
toMockPeers (WorldState m) = HS.fromMap (() <$ m)

-- | A mock 'NetworkLayer' operating in a mock/simulated environment.
mockLayer
    :: ( Eq (Crypto.PublicKey MockCrypto)
       , Hashable (Crypto.PublicKey MockCrypto)
       , Serialise (BlockHash MockCrypto)
       )
    => NetworkLayer MockCrypto Sim
mockLayer =
    NetworkLayer
        { nlActivePeers = gets toMockPeers
        , nlDataFetcher = mkDataFetcherSim
        , nlEventTracer = probed noProbe
        }

-- | Creates a new simulation 'DataFetcher'.
mkDataFetcherSim
    :: ( Eq (Crypto.PublicKey MockCrypto)
       , Hashable (Crypto.PublicKey MockCrypto)
       , Serialise (BlockHash MockCrypto)
       )
    => DataFetcher MockCrypto Sim
mkDataFetcherSim = DataFetcher $ \peer -> \case
    SGetTip -> \() -> do
        WorldState{..} <- get
        case HM.lookup peer _mockPeerState of
            Nothing -> panic "newDataFetcherSim - precondition violation, empty blockchain."
            Just bc -> pure $ GetTipResp (tip bc)
    SGetBlocks -> \Range{..} -> notImplemented
    SGetBlockHeaders -> \Range{..} -> notImplemented

{------------------------------------------------------------------------------
  Running a Sync operation
------------------------------------------------------------------------------}

-- | Runs a 'Sync' operation on the simulated environment.
runMockSync
    :: WorldState
    -> NetworkLayer MockCrypto Sim
    -> Sync MockCrypto Sim a
    -> Either (SyncError MockCrypto) a
runMockSync ws netLayer (Sync s) =
    runIdentity . flip evalStateT ws
                . runReaderT (runExceptT s)
                $ netLayer
