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
module Oscoin.P2P.Disco
    ( Options(..)
    , discoParser

    , DiscoEvent(..)

    , withDisco
    ) where

import           Oscoin.Prelude hiding (option)

import           Oscoin.Crypto (Crypto)
import qualified Oscoin.P2P.Disco.MDns as MDns
import           Oscoin.P2P.Types
                 ( Host
                 , Network
                 , NodeAddr(..)
                 , SeedAddr
                 , hostEither
                 , hostnameToDomain
                 , namedHost
                 , readHostnameText
                 , readNetwork
                 , readNodeAddr
                 , renderNetwork
                 )

import           Data.IP
import qualified Data.Set as Set
import           Lens.Micro.Extras (view)
import qualified Network.DNS as DNS
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
import           Options.Applicative
import           Options.Applicative.Help
                 (paragraph, stringChunk, unChunk, vcatChunks, vsepChunks)

{-# ANN module ("HLint: ignore Use >=>" :: String) #-}

data Options = Options
    { optNetwork    :: Network
    , optSeeds      :: [SeedAddr Crypto]
    , optSDDomains  :: [HostName]
    , optEnableMDns :: Bool
    , optNameserver :: Maybe (HostName, PortNumber) -- only for testing currently
    }

discoParser :: Parser Options
discoParser = Options
    <$> option (eitherReader readNetwork)
        ( long "network"
       <> help "The name of the oscoin network to participate in"
        )
    <*> many
        ( option (eitherReader readNodeAddr)
            ( long "seed"
           <> helpDoc
               ( unChunk $ vsepChunks
               [ paragraph "Zero or more gossip seed nodes to connect to"
               , paragraph "If HOST is an IPv6 address, it must be enclosed in \
                           \square brackets to delimit it from the portnumber."
               , paragraph "If HOST is a domain name, all IPv4 and IPv6 \
                           \addresses bound to it will be considered as \
                           \distinct peer addresses."
               , paragraph "Examples:"
               , vcatChunks $ map stringChunk
                    [ "--seed=\"[2001:db8::01]:6942\""
                    , "--seed=\"127.0.0.1:6942\""
                    , "--seed=testnet.oscoin.io:6942\""
                    ]
               ]
               )
           <> metavar "HOST:PORT"
            )
        )
    <*> many
        ( option str
            ( long "sd-domain"
           <> helpDoc
                ( unChunk $ vsepChunks
                [ paragraph "Zero or more search domains to query for SRV records"
                , paragraph "Examples:"
                , vcatChunks $ map stringChunk
                    [ "--sd-domain=svc.cluster.local"
                    , "--sd-domain=oscoin.io"
                    , "--sd-domain=monadic.xyz"
                    ]
                ]
                )
           <> metavar "DOMAIN NAME"
            )
        )
    <*> switch
        ( long "enable-mdns"
       <> help "Enable mDNS discovery"
        )
    <*> pure Nothing

data DiscoEvent =
      MDnsResponderEvent MDns.ResponderTrace
    | MDnsResolverEvent  MDns.ResolverTrace
    | AddrInfoError      IP PortNumber IOException
    | DNSError           DNS.DNSError
    deriving Show

-- | Set up the discovery machinery, and pass an a continuation to perform
-- actual discovery.
--
-- Note that, if 'optEnableMDns' is 'True', this will also start the mDNS
-- responder at 'Oscoin.P2P.Disco.MDns.defaultMCastAddr'.
--
withDisco
    :: (HasCallStack => DiscoEvent -> IO ())
    -> Options
    -> Set MDns.Service
    -> (IO (Set SockAddr) -> IO a)
    -> IO a
withDisco tracer opt !advertise k = do
    rs <-
        DNS.makeResolvSeed
            . maybe identity
                    (\ns -> \rc -> rc { DNS.resolvInfo = uncurry DNS.RCHostPort ns })
                    (optNameserver opt)
            $ DNS.defaultResolvConf
                { DNS.resolvCache = Just DNS.defaultCacheConf }

    run $ k (resolve rs)
  where
    run = if optEnableMDns opt then withResponder else identity

    resolve rs = do
        addrs <-
            liftA2 (<>)
                   (map Set.fromList
                        . flip concatMapM (optSDDomains opt)
                        $ resolveSRV tracer rs (optNetwork opt))
                   (map Set.fromList
                        . flip concatMapM (optSeeds opt)
                        $ \NodeAddr { nodeHost, nodePort } ->
                            resolveA tracer rs nodeHost nodePort)

        if optEnableMDns opt then do
            msrvs <- do
                mrs <- MDns.newResolver $ tracer . MDnsResolverEvent
                Set.fromList <$> resolveMDns tracer rs mrs (optNetwork opt)
            pure $ addrs <> msrvs
        else
            pure addrs

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
