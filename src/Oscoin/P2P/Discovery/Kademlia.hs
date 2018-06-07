{-# LANGUAGE LambdaCase #-}

module Oscoin.P2P.Discovery.Kademlia
    ( Config (..)
    , mkConfig

    , mkDisco
    ) where

import           Oscoin.P2P.Discovery.Internal (Disco(..))
import           Oscoin.Prelude

import           Control.Exception.Safe (Exception, throwM)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import qualified Network.Kademlia as K
import           Network.Socket (HostName)

-- | Wrapper providing a 'K.Serialize' instance for any type 'a' which has a
-- 'Binary' instance.
newtype KSerialize a = KSerialize a
    deriving (Eq, Show, Ord)

instance Binary a => K.Serialize (KSerialize a) where
    fromBS bs = case Binary.decodeOrFail (LBS.fromStrict bs) of
        Left  (_, _, str)        -> Left str
        Right (unconsumed, _, i) -> Right (KSerialize i, LBS.toStrict unconsumed)
    toBS (KSerialize i) = LBS.toStrict . Binary.encode $ i

data Config i = Config
    { cfgBindAddress     :: (HostName, Word16)
    , cfgExternalAddress :: (HostName, Word16)
    , cfgNodeId          :: i
    }

data KademliaDiscoError =
      NodeBanned
    | IDClash
    deriving (Eq, Show)

instance Exception KademliaDiscoError

mkConfig :: (HostName, Word16) -> (HostName, Word16) -> i -> Config i
mkConfig cfgBindAddress cfgExternalAddress cfgNodeId = Config{..}

mkDisco
    :: forall i. (Binary i, Show i, Ord i)
    => Config i
    -> K.Peer
    -> IO (Disco IO K.Peer)
mkDisco Config{..} peer = do
    k <- newKademliaInstance
    K.joinNetwork k peer >>= \case
        K.NodeBanned  -> throwM NodeBanned
        K.IDClash     -> throwM IDClash
        K.NodeDown    -> putStrLn "Initial peer down"
        K.JoinSuccess -> pure ()

    pure Disco
        { knownPeers = Set.fromList . map (K.peer . fst) <$> K.dumpPeers k
        , closeDisco = K.close k
        }
  where
    newKademliaInstance :: IO (K.KademliaInstance (KSerialize i) (KSerialize Int))
    newKademliaInstance =
        K.createL cfgBindAddress
                  cfgExternalAddress
                  (KSerialize cfgNodeId)
                  K.defaultConfig
                  putStrLn
                  putStrLn
