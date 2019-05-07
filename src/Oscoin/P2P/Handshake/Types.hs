module Oscoin.P2P.Handshake.Types
    ( Handshake
    , HandshakeRole (..)

    , HandshakeT
    , runHandshakeT
    , withHandshakeT
    , handshakeSend
    , handshakeRecv
    , modifyTransport

    , HandshakeResult (..)
    , mapInput
    , mapOutput
    , mapInfo
    )
where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import           Control.Monad.State (modify')
import qualified Oscoin.P2P.Transport as Transport

data HandshakeRole = Acceptor | Connector

type Handshake e n i o =
       HandshakeRole
    -> Maybe n       -- ^ 'Just' if the peer id is known and must match 'hrPeerId'
    -> HandshakeT e IO (HandshakeResult i o n)

newtype HandshakeT e m a = HandshakeT
    { fromHandshakeT :: ExceptT e (StateT Transport.Framed m) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadError e
               )

instance MonadTrans (HandshakeT e) where
    lift = HandshakeT . lift . lift
    {-# INLINE lift #-}

runHandshakeT :: Monad m => Transport.Framed -> HandshakeT e m a -> m (Either e a)
runHandshakeT t = flip evalStateT t . runExceptT . fromHandshakeT

withHandshakeT :: Functor m => (e -> e') -> HandshakeT e m a -> HandshakeT e' m a
withHandshakeT f = HandshakeT . withExceptT f . fromHandshakeT

handshakeSend :: (Serialise a, MonadIO m) => a -> HandshakeT e m ()
handshakeSend a = HandshakeT $ get >>= liftIO . flip Transport.framedSend a

handshakeRecv
    :: (Serialise a, MonadIO m)
    => (Transport.RecvError -> e)
    -> HandshakeT e m a
handshakeRecv f = HandshakeT $
    get >>= withExceptT f . ExceptT . liftIO . Transport.framedRecv

modifyTransport
    :: Monad m
    => (Transport.Framed -> Transport.Framed)
    -> HandshakeT e m ()
modifyTransport f = HandshakeT $ modify' f

data HandshakeResult i o n = HandshakeResult
    { hrPeerInfo :: n
    , hrPreSend  :: i -> IO o
    , hrPostRecv :: o -> IO i
    } deriving Functor

mapInput
    :: (i' -> IO i )
    -> (i  -> IO i')
    -> HandshakeResult i  o n
    -> HandshakeResult i' o n
mapInput f g hr = hr
    { hrPreSend  = f >=> hrPreSend hr
    , hrPostRecv = hrPostRecv hr >=> g
    }

mapOutput
    :: (o  -> IO o')
    -> (o' -> IO o )
    -> HandshakeResult i o  n
    -> HandshakeResult i o' n
mapOutput f g hr = hr
    { hrPreSend  = hrPreSend hr >=> f
    , hrPostRecv = g >=> hrPostRecv hr
    }

mapInfo
    :: (n  -> n')
    -> HandshakeResult i o n
    -> HandshakeResult i o n'
mapInfo = map
