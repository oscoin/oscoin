module Test.Oscoin.P2P.Helpers
    ( framedPair
    , streamingPair

    , streamingServer
    , streamingClient
    , framedServer
    , framedClient
    , hybridServer
    , hybridClient

    , bind
    , accept
    , connect
    ) where

import           Oscoin.Prelude

import qualified Oscoin.P2P.Transport as Transport

import           Codec.Serialise (Serialise)
import           Data.Conduit (runConduit, (.|))
import           Data.Conduit.Combinators (sinkList)
import           Data.Conduit.TMChan
                 ( TMChan
                 , closeTMChan
                 , dupTMChan
                 , newBroadcastTMChan
                 , readTMChan
                 , sourceTMChan
                 , writeTMChan
                 )
import           Data.Streaming.Network
                 (acceptSafe, bindRandomPortTCP, getSocketTCP)
import           Network.Socket (SockAddr, Socket)
import qualified Network.Socket as Sock

-- Mock Transport --------------------------------------------------------------

framedPair :: IO ((Transport.Framed, IO ()), (Transport.Framed, IO ()))
framedPair = do
    ((write1, read1), (write2, read2)) <- chanPair
    let
        write ch = atomically . writeTMChan ch
        read  ch = atomically . map (note Transport.RecvTimeout) $ readTMChan ch
        close ch = atomically $ closeTMChan ch
        t1       = Transport.anyFramed (write write1) (read read2)
        t2       = Transport.anyFramed (write write2) (read read1)
     in
         pure ((t1, close write1), (t2, close write2))

streamingPair :: IO ((Transport.Streaming a, IO ()), (Transport.Streaming a, IO ()))
streamingPair = do
    ((write1, read1), (write2, read2)) <- chanPair
    let
        write ch = atomically . writeTMChan ch
        read  ch = sourceTMChan ch
        close ch = atomically $ closeTMChan ch
        t1       = Transport.anyStreaming (write write1) (read read2)
        t2       = Transport.anyStreaming (write write2) (read read1)
     in
         pure ((t1, close write1), (t2, close write2))

chanPair :: IO ((TMChan a, TMChan a), (TMChan a, TMChan a))
chanPair = atomically $ do
    (write1, write2) <- liftA2 (,) newBroadcastTMChan newBroadcastTMChan
    (read1,  read2 ) <- liftA2 (,) (dupTMChan write1) (dupTMChan write2)
    pure ((write1, read1), (write2, read2))

-- IO --------------------------------------------------------------------------

streamingServer :: Serialise a => Socket -> IO [a]
streamingServer sock =
    accept sock $ \(sock',_) ->
        let tsp = Transport.streaming sock'
         in runConduit $ Transport.streamingRecv tsp .| sinkList

streamingClient :: (Foldable t, Serialise a) => Int -> t a -> IO ()
streamingClient port msgs =
    connect port $ \(sock,_) ->
        let tsp = Transport.streaming sock
         in traverse_ (Transport.streamingSend tsp) msgs

framedServer :: Serialise a => Socket -> IO [a]
framedServer sock =
    accept sock $ \(sock',_) ->
        map reverse . loop [] $ Transport.framed sock'
  where
    loop acc tsp = do
        recv'd <- Transport.framedRecv tsp
        case recv'd of
            Left  Transport.RecvConnReset -> pure acc
            Left  e                       -> throwM e
            Right frame                   -> loop (frame : acc) tsp

framedClient :: (Foldable t, Serialise a) => Int -> t a -> IO ()
framedClient port msgs =
    connect port $ \(sock,_) ->
        let tsp = Transport.framed sock
         in traverse_ (Transport.framedSend tsp) msgs

hybridServer :: Serialise a => Int -> Socket -> IO [a]
hybridServer nframed sock=
    accept sock $ \(sock',_) -> do
        as <- map reverse . loop nframed [] $ Transport.framed sock'
        bs <-
            runConduit $
                   Transport.streamingRecv (Transport.streaming sock')
                .| sinkList
        pure $ as <> bs
  where
    loop 0 acc _   = pure acc
    loop n acc tsp = do
        recv'd <- Transport.framedRecv tsp
        case recv'd of
            Left  Transport.RecvConnReset -> pure acc
            Left  e                       -> throwM e
            Right frame                   -> loop (n - 1) (frame : acc) tsp

hybridClient :: (Foldable t, Serialise a) => Int -> t a -> t a -> IO ()
hybridClient port framed streaming =
    connect port $ \(sock,_) ->
        let
            tf = Transport.framed sock
            ts = Transport.streaming sock
         in
            traverse_ (Transport.framedSend    tf) framed
         *> traverse_ (Transport.streamingSend ts) streaming

bind :: ((Int, Sock.Socket) -> IO a) -> IO a
bind = bracket (bindRandomPortTCP "127.0.0.1") (Sock.close . snd)

accept :: Socket -> ((Socket, SockAddr) -> IO a) -> IO a
accept sock = bracket (acceptSafe sock) (Sock.close . fst)

connect :: Int -> ((Socket, SockAddr) -> IO a) -> IO a
connect port = bracket (getSocketTCP "127.0.0.1" port) (Sock.close . fst)
