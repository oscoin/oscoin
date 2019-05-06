-- | Peer discovery
--
-- Peer addresses may be given statically ('optSeeds') as either @ip:port@ or
-- @domain:port@ pairs. In the latter case, /all/ IPv4 and IPv6 addresses
-- returned by the DNS will be considered as distinct peer addresses.
--
-- Seed nodes running on non-standard ports may advertise themselves via SRV
-- records of the form:
--
--     @_gossip._tcp.oscoin.<network>.<search domain>@
--
-- respectively
--
--     @_api._tcp.oscoin.<network>.<search domain>@
--
-- where @<network>@ is a logical network name as defined by
-- 'Oscoin.P2P.Types.Network', and @<search domain>@ is just a domain name owned
-- by the node provider (e.g. \"oscoin.io\"). Which search domains to consider
-- is specified by 'optSDDomains'.
--
-- For convenience, if 'optEnableMDns' is 'True', a multicast DNS server is
-- started on 'Oscoin.P2P.Disco.MDns.defaultMCastAddr', which responds to SRV
-- queries of the same form as above with a @<search domain>@ of \".local.\".
-- 'withDisco' also queries the multicast group for local area peers.
--
-- DNSSEC is not currently supported.
--
-- When running on Google Compute Engine (GCE), and 'optEnableGCE' is 'True', it
-- is attempted to find similar instances in the same VPC (see
-- "Oscoin.P2P.Disco.GCE").
--
module Oscoin.P2P.Disco
    ( DiscoEvent(..)

    , withDisco

    -- * Re-exports
    , module Oscoin.P2P.Disco.Options
    ) where

import           Oscoin.Prelude hiding (option)

import qualified Oscoin.P2P.Disco.GCE as GCE
import qualified Oscoin.P2P.Disco.MDns as MDns
import           Oscoin.P2P.Disco.Options
import           Oscoin.P2P.Types
                 ( Host
                 , Network
                 , NodeAddr(..)
                 , hostEither
                 , hostnameToDomain
                 , namedHost
                 , readHostnameText
                 , renderNetwork
                 )

import qualified Data.HashMap.Strict as Map
import           Data.IP
import qualified Data.Set as Set
import           Lens.Micro (set)
import           Lens.Micro.Extras (view)
import qualified Network.DNS as DNS
import qualified Network.Google as Goog
import qualified Network.Google.Compute as Goog (computeReadOnlyScope)
import qualified Network.Google.Compute.Metadata as Goog (getProjectId)
import           Network.Socket
                 ( AddrInfo(..)
                 , AddrInfoFlag(AI_ADDRCONFIG, AI_ALL, AI_NUMERICHOST)
                 , Family(AF_INET, AF_INET6)
                 , HostName
                 , PortNumber
                 , SockAddr
                 , SocketType(Stream)
                 , defaultHints
                 , getAddrInfo
                 )

{-# ANN module ("HLint: ignore Use >=>" :: String) #-}

data DiscoEvent =
      MDnsResponderEvent MDns.ResponderTrace
    | MDnsResolverEvent  MDns.ResolverTrace
    | AddrInfoError      IP PortNumber IOException
    | DNSError           DNS.DNSError
    deriving Show

-- | Set up the discovery machinery, and pass an action to the continuation to
-- perform actual discovery.
--
-- Note that, if 'optEnableMDns' is 'True', this will also start the mDNS
-- responder at 'Oscoin.P2P.Disco.MDns.defaultMCastAddr'.
--
withDisco
    :: (HasCallStack => DiscoEvent -> IO ())
    -> Options crypto Network
    -> PortNumber
    -> Set MDns.Service
    -> (IO (Set SockAddr) -> IO a)
    -> IO a
withDisco tracer opt defaultGossipPort !advertise k = do
    rslv <-
        DNS.makeResolvSeed
            . maybe identity
                    (\ns -> \rc -> rc { DNS.resolvInfo = uncurry DNS.RCHostPort ns })
                    (optNameserver opt)
            $ DNS.defaultResolvConf
                { DNS.resolvCache = Just DNS.defaultCacheConf }

    mrslv <-
        if optEnableMDns opt then
            Just <$> MDns.newResolver (tracer . MDnsResolverEvent)
        else
            pure Nothing

    goog <-
        if optEnableGCE opt then
                Just
             .  set Goog.envScopes Goog.computeReadOnlyScope
             .  set Goog.envLogger (\_ _ -> pure ())
            <$> Goog.newEnv
        else
            pure Nothing

    run $ k (resolve rslv mrslv goog)
  where
    run = if optEnableMDns opt then withResponder else identity

    resolve rslv mrslv goog = runConcurrently $ (\a b c d -> a <> b <> c <> d)
        <$> (Concurrently
                . map Set.fromList
                . flip concatMapM (optSDDomains opt)
                $ resolveSRV tracer rslv (optNetwork opt))
        <*> (Concurrently
                . map Set.fromList
                . flip concatMapM (optSeeds opt)
                $ \NodeAddr { nodeHost, nodePort } ->
                    resolveA tracer rslv nodeHost nodePort)
        <*> (Concurrently $
                map (fromMaybe mempty) . for mrslv $ \r ->
                    Set.fromList <$>
                        resolveMDns tracer rslv r (optNetwork opt))
        <*> (Concurrently $
                map (fromMaybe mempty) . for goog $ \g -> do
                    -- TODO(kim): we may want to allow passing in the project
                    -- id, so we're not limited to running GCE disco on GCE only
                    -- (although the utility of that is debatable)
                    proj <- Goog.getProjectId (view Goog.envManager g)
                    rs   <-
                        Set.toList
                            <$> GCE.lookup proj
                                           gceLabels
                                           gcePortLabel
                                           (optNetwork opt)
                                           g
                    map Set.fromList . flip concatMapM rs $ \(ip, port) ->
                        getSockAddrs tracer
                                     ip
                                     (fromMaybe defaultGossipPort port))

    gceLabels    = Map.fromList [("app", "oscoin-node")]
    gcePortLabel = "gossipPort"

    withResponder io =
        withAsync runResponder $ \r ->
        withAsync io           $ \i ->
            waitEitherCatchCancel r i >>=
                either
                    (either throwIO (const $ throwString "mDNS responder died unexpectedly"))
                    (either throwIO pure)

    runResponder =
        MDns.runResponder =<<
            MDns.newResponder (tracer . MDnsResponderEvent)
                              (optNetwork opt)
                              advertise

-- Internal --------------------------------------------------------------------

resolveSRV
    :: (HasCallStack => DiscoEvent -> IO ())
    -> DNS.ResolvSeed
    -> Network
    -> HostName
    -> IO [SockAddr]
resolveSRV tracer rs net searchDomain =
    DNS.withResolver rs $ \r -> do
        -- FIXME(kim): also consider the additional section for resolved A/AAAA
        -- records. Need to ping kazu to allow caching \"raw\" lookups
        srvs <- DNS.lookupSRV r $ toS srvDomain
        case srvs of
            Left  e  -> [] <$ tracer (DNSError e)
            Right ss -> flip concatMapM ss $ \(_,_, port, dom) ->
                case namedHost <$> readHostnameText (toS dom) of
                    Left  e -> [] <$ tracer (DNSError (DNS.DecodeError e))
                    Right h -> resolveA tracer rs h (fromIntegral port)
  where
    srvDomain =
           "_gossip._tcp.oscoin."
        <> renderNetwork net
        <> "."
        <> toS searchDomain

resolveA
    :: (HasCallStack => DiscoEvent -> IO ())
    -> DNS.ResolvSeed
    -> Host
    -> PortNumber
    -> IO [SockAddr]
resolveA tracer rs host port = case view hostEither host of
    Left  ip -> getSockAddrs tracer ip port
    Right hn -> DNS.withResolver rs $ \r -> do
        let dom = hostnameToDomain hn
        ip4s <- second (map IPv4) <$> DNS.lookupA    r dom
        ip6s <- second (map IPv6) <$> DNS.lookupAAAA r dom
        case partitionEithers [ip4s, ip6s] of
            (errs, ips) -> do
                traverse_ (tracer . DNSError) errs
                flip concatMapM (concat ips) $ \ip ->
                    getSockAddrs tracer ip port

resolveMDns
    :: (HasCallStack => DiscoEvent -> IO ())
    -> DNS.ResolvSeed
    -> MDns.Resolver
    -> Network
    -> IO [SockAddr]
resolveMDns tracer rs mrs net = do
    srvs <- MDns.lookup mrs MDns.defaultLookupOptions net "gossip" MDns.TCP
    concatMapM (uncurry (resolveA tracer rs)) $ toList srvs

getSockAddrs
    :: (HasCallStack => DiscoEvent -> IO ())
    -> IP
    -> PortNumber
    -> IO [SockAddr]
getSockAddrs tracer ip port =
    handleIO (const $ pure mempty)                      $
    flip withException (tracer . AddrInfoError ip port) $
        map addrAddress
            <$> getAddrInfo (Just hints) (Just (show ip)) (Just (show port))
  where
    hints = defaultHints
        { addrFlags      = [AI_ADDRCONFIG, AI_ALL, AI_NUMERICHOST]
        , addrFamily     = case ip of
            IPv4{} -> AF_INET
            IPv6{} -> AF_INET6
        , addrSocketType = Stream
        }
