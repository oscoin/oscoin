{-# LANGUAGE DataKinds #-}

module Oscoin.P2P.Disco.GCE where

import           Oscoin.Prelude

import           Oscoin.P2P.Types (Network, renderNetwork)

import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.IP (IP)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Read as T
import           Lens.Micro
                 ( Getting
                 , each
                 , filtered
                 , set
                 , to
                 , toListOf
                 , traversed
                 , (^?)
                 , _Just
                 )
import           Lens.Micro.Extras (view)
import           Network.Google
                 (AllowScopes, Env, HasScope, runGoogle, runResourceT, send)
import           Network.Google.Compute
                 ( Instance
                 , InstanceAggregatedList
                 , InstancesAggregatedList
                 , NetworkInterface
                 , i1Labels
                 , i1NetworkInterfaces
                 , ialItems
                 , ialaFilter
                 , ialiAddtional
                 , ilAddtional
                 , instancesAggregatedList
                 , islInstances
                 , niNetwork
                 , niNetworkIP
                 )
import           Network.Socket (PortNumber)

-- | Look up GCE instances as gossip peers.
--
-- The instances are filtered by the given labels. Each instance must be on the
-- VPC denoted by 'Network', which is determined by:
--
-- * filtering by a "network" label with a value of 'Network'
-- * considering only the NIC(s) of instances with a matching 'niNetwork'
--
-- Instances may advertise their bound ports via labels.
--
lookup
    :: ( MonadUnliftIO m
       , AllowScopes s
       , HasScope    s InstancesAggregatedList
       )
    => Text              -- ^ Project ID
    -> HashMap Text Text -- ^ Labels
    -> Text              -- ^ Label to consider for obtaining 'PortNumber'
    -> Network
    -> Env s
    -> m (Set (IP, Maybe PortNumber))
lookup proj labels portLabel (renderNetwork -> net) r =
    runResourceT . runGoogle r $ do
        instances <- toListOf inst <$> send (mkRq proj)
        pure . Set.fromList . flip concatMap instances $ \i ->
            let
                iport = firstOf  (port . traversed) i
                addrs = toListOf addr               i
             in
                zip addrs (repeat iport)
  where
    -- morally. microlens is just .. too micro
    firstOf = flip (^?)

    filters :: Text
    filters =
          mconcat
        . intersperse " AND "
        . map (\(k, v) -> "(labels." <> k <> " = " <> v <> ")")
        . Map.toList
        . Map.insert "network" net
        $ labels

    mkRq :: Text -> InstancesAggregatedList
    mkRq = set ialaFilter (Just filters) . instancesAggregatedList

    inst :: Monoid r => Getting r InstanceAggregatedList Instance
    inst =
          ialItems . _Just
        . ialiAddtional
        . traversed . islInstances . traversed

    addr :: Monoid r => Getting r Instance IP
    addr =
          i1NetworkInterfaces
        . each . filtered inVPC
        . niNetworkIP . _Just
        . to (readMaybe . T.unpack) . _Just

    port :: Monoid r => Getting r Instance (Maybe PortNumber)
    port =
          i1Labels . _Just
        . ilAddtional
        . to portFromLabels

    inVPC :: NetworkInterface -> Bool
    inVPC nic =
        case view niNetwork nic of
            Nothing -> False
            Just  n -> net `T.isSuffixOf` n

    portFromLabels :: HashMap Text Text -> Maybe PortNumber
    portFromLabels =
          map (fromIntegral @Word16 . fst)
        . (>>= hush . T.decimal)
        . Map.lookup portLabel
