{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#include <HsNet.h>

-- | Multcast DNS discovery
--
-- This does not currently implement the full mDNS (RFC 6762) resp. DNS-SD (RFC
-- 6763) specs, but simply allows advertising / resolving SRV records conforming
-- to the same format as described in "Oscoin.P2P.Disco".
--
module Oscoin.P2P.Disco.MDns
    ( Proto(..)
    , Service(..)
    , ResponderTrace(..)
    , ResolverTrace(..)

    , Responder
    , responderLocalHost
    , responderNetwork
    , responderServices
    , responderMCastAddr
    , responderTrace
    , newResponder
    , runResponder

    , Resolver
    , resolverNameserverAddr
    , resolverTrace
    , newResolver

    , LookupOptions(..)
    , defaultLookupOptions
    , lookup
    ) where

import           Oscoin.Prelude

import           Oscoin.P2P.Types
                 ( Host
                 , Network
                 , domainToHostname
                 , eitherToHost
                 , numericHost
                 , renderNetwork
                 )
import           Oscoin.Time (Duration, seconds)

import           Control.Monad.Trans.State (modify')
import           Crypto.Random (ChaChaDRG, drgNew, randomBytesGenerate)
import qualified Data.ByteString as BS (unpack)
import           Data.IP (IP(..))
import qualified Data.IP as IP
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Foreign.C.Error
import           Foreign.C.Types
import           Foreign.Marshal hiding (void)
import           Foreign.Ptr
import           Foreign.Storable
import qualified Network.DNS as DNS
import           Network.HostName (getHostName)
import           Network.Socket
                 ( AddrInfo(..)
                 , AddrInfoFlag(AI_ADDRCONFIG, AI_ALL, AI_NUMERICHOST)
                 , Family(AF_INET)
                 , HostAddress
                 , HostAddress6
                 , HostName
                 , PortNumber
                 , SockAddr(..)
                 , Socket
                 , SocketOption(..)
                 , SocketType(Datagram, Stream)
                 , bind
                 , close
                 , defaultHints
                 , fdSocket
                 , getAddrInfo
                 , setSocketOption
                 , socket
                 )
#if MIN_VERSION_network(3,0,0)
import           Network.Socket.Address (SocketAddress(..))
#else
import           Network.Socket.Internal (withSockAddr)
#endif
import           Network.Socket.ByteString (recvFrom, sendAllTo)
import           System.Clock
                 ( Clock(Monotonic)
                 , diffTimeSpec
                 , getTime
                 , toNanoSecs
                 )
import qualified System.Timeout as System

{-# ANN module ("Hlint: Ignore Use camelCase" :: String) #-}

data ResponderTrace =
      ResponderError SomeException
    | ResponderWhatsTheQuestion [DNS.Question] SockAddr
    | ResponderHaveNoAnswer     DNS.Question   SockAddr
    | ResponderRecv             DNS.DNSMessage SockAddr
    | ResponderSend             DNS.DNSMessage SockAddr
    deriving Show

data Responder = Responder
    { responderLocalHost :: HostName
    , responderNetwork   :: Network
    , responderServices  :: Set Service
    , responderMCastAddr :: (HostName, PortNumber)
    , responderTrace     :: HasCallStack => ResponderTrace -> IO ()
    }

data Proto = TCP | UDP
    deriving (Eq, Ord)

renderProto :: Proto -> Text
renderProto TCP = "_tcp"
renderProto UDP = "_udp"

newtype ServiceName = ServiceName Text
    deriving (Eq, Ord, IsString)

data Service = Service
    { srvName  :: ServiceName
    , srvProto :: Proto
    , srvHost  :: IP
    , srvPort  :: PortNumber
    } deriving (Eq, Ord)

renderSrvName :: ServiceName -> Text
renderSrvName (ServiceName name) =
    case Text.stripPrefix "_" name of
        Nothing -> "_" <> name
        Just _  -> name

serviceDomain :: Network -> ServiceName -> Proto -> DNS.Domain
serviceDomain net srvname proto = encodeUtf8 $
       renderSrvName srvname
    <> "."
    <> renderProto proto
    <> ".oscoin."
    <> renderNetwork net
    <> ".local."

defaultMCastAddr :: (HostName, PortNumber)
defaultMCastAddr = ("224.0.0.251", 5152)

newResponder
    :: (HasCallStack => ResponderTrace -> IO ())
    -> Network
    -> Set Service
    -> IO Responder
newResponder responderTrace responderNetwork responderServices = do
    responderLocalHost <- getHostName
    let responderMCastAddr = defaultMCastAddr
    pure Responder {..}

runResponder :: Responder -> IO a
runResponder rsp = do
    lolRRs <-
          fmap (mapMaybe mkLolRR . nub)
        . concatMapM (uncurry getSrvAddr)
        . toList
        . Set.map (\Service { srvHost, srvProto } -> (srvHost, srvProto))
        $ responderServices rsp

    uncurry withListenSocket (responderMCastAddr rsp) $ \sock -> forever $
        flip withException (responderTrace rsp . ResponderError) $ do
            (msg,addr) <- receive sock
            void . forkIO $ do
                responderTrace rsp $ ResponderRecv msg addr
                case filter ((== DNS.SRV) . DNS.qtype) $ DNS.question msg of
                    q:_ ->
                        case Map.lookup (DNS.qname q) answers of
                            Nothing -> responderTrace rsp $
                                ResponderHaveNoAnswer q addr
                            Just rr -> respond sock addr $
                                mkResponse lolRRs
                                           (DNS.identifier (DNS.header msg))
                                           q
                                           rr
                    _ -> responderTrace rsp $
                        ResponderWhatsTheQuestion (DNS.question msg) addr
  where
    -- Nb. we respond via unicast to the requestor. Some firewalls block this
    -- (for good reasons), but this way we can conveniently @dig@ for the first
    -- node to respond. We should potentially honour the \"QU\"/\"QM\" bit (RFC
    -- 6762, Section 5.4).
    respond :: Socket -> SockAddr -> DNS.DNSMessage -> IO ()
    respond sock to resp = do
        responderTrace rsp $ ResponderSend resp to
        sendAllTo sock (DNS.encode resp) to

    lol = toS $ responderLocalHost rsp <> ".local."

    answers =
          Map.fromList
        . flip map (toList (responderServices rsp))
        $ \Service { srvName, srvProto, srvPort } ->
            let
                dom = serviceDomain (responderNetwork rsp) srvName srvProto
                rr  = DNS.ResourceRecord
                    { DNS.rrname  = dom
                    , DNS.rrtype  = DNS.SRV
                    , DNS.rrclass = DNS.classIN
                    , DNS.rrttl   = 300
                    , DNS.rdata   = DNS.RD_SRV 10 10 (fromIntegral srvPort) lol
                    }
             in
                (dom, rr)

    getSrvAddr :: IP -> Proto -> IO [SockAddr]
    getSrvAddr srvHost srvProto =
        let
            hints = defaultHints
                    { addrFlags      = [AI_ADDRCONFIG, AI_ALL, AI_NUMERICHOST]
                    , addrFamily     = AF_INET
                    , addrSocketType = case srvProto of
                        TCP -> Stream
                        UDP -> Datagram
                    }
         in
            map addrAddress
                <$> getAddrInfo (Just hints) (Just (show srvHost)) Nothing

    mkLolRR = \case
        SockAddrInet _ addr -> pure DNS.ResourceRecord
            { DNS.rrname  = lol
            , DNS.rrtype  = DNS.A
            , DNS.rrclass = DNS.classIN
            , DNS.rrttl   = 300
            , DNS.rdata   = DNS.RD_A (IP.fromHostAddress addr)
            }
        SockAddrInet6 _ _ addr _ -> pure DNS.ResourceRecord
            { DNS.rrname  = lol
            , DNS.rrtype  = DNS.AAAA
            , DNS.rrclass = DNS.classIN
            , DNS.rrttl   = 300
            , DNS.rdata   = DNS.RD_AAAA (IP.fromHostAddress6 addr)
            }

        _ -> Nothing

    mkResponse lolRRs ident q ans = DNS.defaultResponse
        { DNS.header     = (DNS.header DNS.defaultResponse)
            { DNS.identifier = ident }
        , DNS.question   = [q]
        , DNS.answer     = [ans]
        , DNS.additional = lolRRs
        }

data Resolver = Resolver
    { resolverNameserverAddr :: AddrInfo
    , _resolverGenId         :: IO DNS.Identifier
    , resolverTrace          :: HasCallStack => ResolverTrace -> IO ()
    -- TODO: caching
    }

data ResolverTrace =
      ResolverError SomeException
    | ResolverRecv  DNS.DNSMessage SockAddr
    deriving Show

newResolver :: (HasCallStack => ResolverTrace -> IO ()) -> IO Resolver
newResolver tracer = do
    drg    <- drgNew >>= newIORef
    addr:_ <-
        let (host, port) = defaultMCastAddr
         in getAddrInfo Nothing (Just host) (Just (show port))
    pure Resolver
        { resolverNameserverAddr = addr
        , _resolverGenId         = getRandom drg
        , resolverTrace          = tracer
        }
  where
    getRandom :: IORef ChaChaDRG -> IO Word16
    getRandom ref = atomicModifyIORef' ref $ \gen ->
      let
          (bs, gen') = randomBytesGenerate 2 gen
          !seqno     = case map fromIntegral (BS.unpack bs) of
              (u:l:_) -> u * 256 + l
              _       -> panic "Too few random bytes"
       in
          (gen', seqno)

data LookupOptions = LookupOptions
    { optExpectPeers :: Int
    -- ^ Expect at most this many peers to respond.
    , optTimeout     :: Duration
    -- ^ Abort the 'lookup' after this many nanoseconds if less than
    -- 'optExpectPeers' responded.
    }

defaultLookupOptions :: LookupOptions
defaultLookupOptions = LookupOptions
    { optExpectPeers = 3
    , optTimeout     = 2 * seconds
    }

-- | mDNS lookup of peer addresses.
--
-- 'LookupOptions' defines how long this function blocks: either
-- 'optExpectPeers' or more addresses have been found, or 'optTimeout' expires,
-- whichever comes first.
lookup
    :: Resolver
    -> LookupOptions
    -> Network
    -> ServiceName
    -> Proto
    -> IO (Set (Host, PortNumber))
lookup Resolver { resolverNameserverAddr = ns
                , _resolverGenId         = genId
                , resolverTrace
                }
       opt net srv proto = do
    ident <- genId
    flip withException (resolverTrace . ResolverError)
        . bracket (socket (addrFamily ns) Datagram 17) close
        $ \sock ->
            let
                dom = serviceDomain net srv proto
                q   = DNS.Question dom DNS.SRV
             in do
                sendAllTo sock
                          (DNS.encodeQuestions ident [q] [] False)
                          (addrAddress ns)
                Set.fromList . concatMap toHostPort <$> loop q sock ident
  where
    toHostPort DNS.DNSMessage { DNS.answer = ans, DNS.additional = add } =
        let
            aRecords :: Map DNS.Domain (Set IP)
            aRecords =
                  foldl' (\m (k, v) -> Map.insertWith (<>) k v m) mempty
                $ flip mapMaybe add $ \rr ->
                    (DNS.rrname rr,) . Set.singleton <$>
                        case DNS.rdata rr of
                            DNS.RD_A    ip -> Just (IPv4 ip)
                            DNS.RD_AAAA ip -> Just (IPv6 ip)
                            _              -> Nothing

            hostname  :: DNS.Domain -> Either String Host
            hostname = map (eitherToHost . Right) . domainToHostname
         in
            flip concatMap ans $ \rr ->
                case DNS.rdata rr of
                    DNS.RD_SRV _ _ port dom' ->
                          map (,fromIntegral port)
                        $ case Map.lookup dom' aRecords of
                            Nothing -> either (const mempty) pure $ hostname dom'
                            Just as -> map numericHost $ toList as
                    _ -> mempty

    loop q sock ident = do
        start <- getTime Monotonic
        flip execStateT [] $
            let
                elapsed = fromIntegral . toNanoSecs . diffTimeSpec start
                go      = do
                    resp <- liftIO $ do
                        now <- getTime Monotonic
                        let
                            timeout = fromIntegral . max 1
                                    $ (optTimeout opt - elapsed now) `div` 1000
                         in
                            System.timeout timeout (receive sock)
                    for_ resp $ \(msg, from) -> do
                        lift . resolverTrace $ ResolverRecv msg from
                        when (respValid q ident msg) $ modify' (msg:)
                    have <- get
                    when (length have < optExpectPeers opt) $ do
                        now <- liftIO $ getTime Monotonic
                        when ((optTimeout opt - elapsed now) > 0) go
             in
                go


    respValid q seqno resp
      | DNS.identifier (DNS.header resp) /= seqno = False
      | [q] /= DNS.question resp                  = False
      | otherwise                                 = True

--------------------------------------------------------------------------------

withListenSocket :: HostName -> PortNumber -> (Socket -> IO a) -> IO a
withListenSocket host port k = do
    info:_ <- getAddrInfo Nothing (Just host) Nothing
    bracket (setup info) close k
  where
    setup info = do
        sock <- socket (addrFamily info) Datagram 17
#if defined (SO_REUSEPORT) && ! defined (linux_HOST_OS)
        setSocketOption sock ReusePort 1
#else
        setSocketOption sock ReuseAddr 1
#endif
        case addrAddress info of
            SockAddrInet _ addr -> do
                bind sock $ SockAddrInet port inaddrAny
                joinGroup sock addr
            SockAddrInet6 _ _ addr _ -> do
                bind sock $ SockAddrInet6 port 0 in6addrAny 0
                joinGroup6 sock addr

            _ -> panic $ "Invalid address: " <> toS host

        pure sock

receive :: Socket -> IO (DNS.DNSMessage, SockAddr)
receive sock = do
    (bs,addr) <- recvFrom sock (fromIntegral DNS.maxUdpSize)
    either throwIO (pure . (,addr)) $ DNS.decode bs

inaddrAny :: HostAddress
inaddrAny = htonl 0

in6addrAny :: HostAddress6
in6addrAny = (0, 0, 0, 0)

foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32
foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

joinGroup :: Socket -> HostAddress -> IO ()
joinGroup sock addr = do
#if MIN_VERSION_network(3,0,0)
    fd <- fdSocket sock
#else
    let fd = fdSocket sock
#endif
    throwErrnoIf_ (/= 0) "joinGroup" $
        allocaBytes #{size struct ip_mreq} $ \mReqPtr -> do
            let iface = #{const INADDR_ANY} `asTypeOf` addr
            #{poke struct ip_mreq, imr_multiaddr} mReqPtr addr
            #{poke struct ip_mreq, imr_interface} mReqPtr iface
            c_setsockopt
                fd
                iPPROTO_IP
                iP_ADD_MEMBERSHIP
                (castPtr mReqPtr)
                (#{size struct ip_mreq})

joinGroup6 :: Socket -> HostAddress6 -> IO ()
joinGroup6 sock addr = do
#if MIN_VERSION_network(3,0,0)
    fd <- fdSocket sock
#else
    let fd = fdSocket sock
#endif
    throwErrnoIf_ (/= 0) "joinGroup6" $
        allocaBytes #{size struct ipv6_mreq} $ \mReqPtr -> do
            withSockAddr (SockAddrInet6 0 0 addr 0) $ \saddr _ -> do
                copyBytes (#{ptr struct ipv6_mreq, ipv6mr_multiaddr} mReqPtr)
                          (#{ptr struct sockaddr_in6, sin6_addr} saddr)
                          #{size struct in6_addr}
            #{poke struct ipv6_mreq, ipv6mr_interface} mReqPtr (0 :: CUInt)
            c_setsockopt
                fd
                iPPROTO_IPV6
                iPV6_ADD_MEMBERSHIP
                (castPtr mReqPtr)
                (#{size struct ipv6_mreq})
#if MIN_VERSION_network(3,0,0)
  where
    withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
    withSockAddr addr f = do
        let sz = sizeOfSocketAddress addr
        allocaBytes sz $ \p -> pokeSocketAddress p addr >> f (castPtr p) sz
#endif

iP_ADD_MEMBERSHIP, iPV6_ADD_MEMBERSHIP :: CInt
iP_ADD_MEMBERSHIP = #const IP_ADD_MEMBERSHIP
#ifdef darwin_HOST_OS
-- WTF: netinet/in.h includes netinet6/in6.h, which definitely defines
-- IPV6_JOIN_GOUP, but for some reason hsc/gcc can't find it.
iPV6_ADD_MEMBERSHIP     = 12
#elif defined (IPV6_JOIN_GOUP)
iPV6_ADD_MEMBERSHIP     = #const IPV6_JOIN_GOUP
#else
iPV6_ADD_MEMBERSHIP     = #const IPV6_ADD_MEMBERSHIP
#endif

iPPROTO_IP, iPPROTO_IPV6 :: CInt
iPPROTO_IP   = #const IPPROTO_IP
iPPROTO_IPV6 = #const IPPROTO_IPV6
