{-# LANGUAGE LambdaCase #-}

-- | Minimalistic logging library with a @printf()@-style interface.
--
-- >>> :{
-- withStdLogger defaultConfig $ \lgr ->
--     info lgr ("Listening on " % string % ":" % int) "localhost" 1234
-- :}
-- Listening on localhost:1234 at="I" loc="interactive:Ghci2:66:39"
--
-- <https://brandur.org/logfmt logfmt>-style "structured" logging is encouraged
-- by convention:
--
-- >>> :{
-- withStdLogger defaultConfig $ \lgr -> do
--     tid <- myThreadId
--     info lgr ("Concurrently " % fthreadId) tid
--  :}
-- Concurrently thread="ThreadId 2196" at="I" loc="interactive:Ghci5:71:39"
module Oscoin.Logging
    ( -- * Types
      Logger
    , MonadLogger
    , Severity (..)
    , Namespace

    -- * Configuration
    , Config (..)
    , defaultConfig

    -- * Constructing 'Logger's
    , withStdLogger
    , stdLogger
    , noLogger

    -- ** Adjusting runtime settings
    , logLevel
    , setLevel

    , logNamespace
    , setNamespace

    -- * Logging in 'IO' with explicit passing of 'Logger'
    , debug
    , info
    , err

    , logException
    , withExceptionLogged

    -- * Logging in 'IO' with implicit passing of 'Logger' via 'MonadReader'
    , debugM
    , infoM
    , errM

    , logExceptionM
    , withExceptionLoggedM

    , withLevel
    , withNamespace
    , withoutLogging

    -- * Logging in pure contexts (via 'MonadWriter')
    , LogRecord
    , Logs

    , debugP
    , infoP
    , errP
    , flush
    , flushM

    -- * Formatters
    , fquoted
    , ftag

    , fexception
    , fcallstack
    , fthreadId
    , fprocessId

    -- ** Re-exports from @formatting@
    , (%)
    , module Export
    ) where

import           Oscoin.Prelude hiding (io)

import           Control.Concurrent (ThreadId)
import           Control.Exception.Safe (Exception, MonadMask, SomeException)
import qualified Control.Exception.Safe as Safe
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Aeson.Encoding as Enc
import           Data.DList (DList, singleton)
import           Data.Int (Int32)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Encoding as LT
import           Formatting (Format, (%))
import qualified Formatting as F
import           GHC.Stack (CallStack, HasCallStack, SrcLoc(..))
import qualified GHC.Stack as Stack
import           Lens.Micro (Lens', lens, set)
import           System.Log.FastLogger (BufSize, LogStr)
import qualified System.Log.FastLogger as FL
import           System.Posix.Types (CPid, ProcessID)

import           Formatting.Formatters as Export

-- | Severity of a log statement
--
-- A log statement is only output if it's 'Severity' is smaller than the
-- 'Logger's. Note that there are only three log levels, or severities.
data Severity =
      Debug -- ^ Things of interest during development, off in a \"production\" setting
    | Info  -- ^ Context around program execution and/or errors
    | Err   -- ^ Errors
    deriving (Eq, Ord, Enum, Show, Read)

-- | An arbitrary namespace
--
-- This gives an additional opportunity for filtering when 'Severity' doesn't
-- quite match, eg. logging in library code may be namespaced to that library's
-- name.
newtype Namespace = Namespace { fromNamespace :: Text }
    deriving IsString

-- | The logging environment
data Logger = Logger
    { _logLvl :: Severity
    , _logNs  :: Maybe Namespace
    , _logOut :: LogStr -> IO ()
    }

-- | Configuration for the 'stdLogger'
data Config = Config
    { cfgLevel     :: Severity
    -- ^ The 'Severity' above which to output log statements. Default: 'Info'
    , cfgNamespace :: Maybe Namespace
    -- ^ A 'Namespace'. Default: 'Nothing'
    , cfgBufSize   :: BufSize
    -- ^ @fast-logger@ buffer size. Default: 'defaultBufSize'
    }

type MonadLogger r m =
    ( MonadReader r m
    , Has Logger r
    , MonadIO m)

-- | Default 'Config'
defaultConfig :: Config
defaultConfig = Config Info Nothing FL.defaultBufSize

-- | Properly 'bracket'ed 'stdLogger'
withStdLogger :: Config -> (Logger -> IO a) -> IO a
withStdLogger cfg f = Safe.bracket (stdLogger cfg) snd (f . fst)

-- | Standard 'Logger' on top of @fast-logger@
--
-- The second element of the returned tuple is a finalizer which flushes the log
-- buffers. Consider using 'bracket' or 'withStdLogger' to guarantee this is
-- called.
stdLogger :: Config -> IO (Logger, IO ())
stdLogger Config{cfgLevel, cfgNamespace, cfgBufSize} = do
    io <- FL.newStderrLoggerSet cfgBufSize
    pure (Logger cfgLevel cfgNamespace (FL.pushLogStrLn io), FL.rmLoggerSet io)

-- | A 'Logger' which logs nothing
noLogger :: Logger
noLogger = Logger Err Nothing (const $ pure ())

-- | Adjust the log level at runtime, as a lens
logLevel :: Lens' Logger Severity
logLevel = lens _logLvl (\s a -> s { _logLvl = a })
{-# INLINE logLevel #-}

-- | Adjust the namespace at runtime, as a lens
logNamespace :: Lens' Logger (Maybe Namespace)
logNamespace = lens _logNs (\s a -> s { _logNs = a })
{-# INLINE logNamespace #-}

-- | Adjust the log level at runtime, as an ordinary function
setLevel :: Severity -> Logger -> Logger
setLevel lvl = set logLevel lvl

-- | Adjust the namespace at runtime, as an ordinary function
setNamespace :: Maybe Namespace -> Logger -> Logger
setNamespace ns = set logNamespace ns

-- IO --------------------------------------------------------------------------

logIO :: Logger -> Severity -> Maybe SrcLoc -> Format (IO ()) a -> a
logIO Logger{..} sev loc fmt
  | sev < _logLvl = F.runFormat fmt (const $ pure ())
  | otherwise     = F.runFormat (fmsg sev loc _logNs fmt) $
                        _logOut . FL.toLogStr . LTB.toLazyText

-- | Log at 'Debug'. The 'SrcLoc' of the call site is automatically added to the
-- log statement.
--
-- >>> :{
-- withStdLogger defaultConfig $ \lgr -> do
--     debug lgr "not printed"
--     info lgr "informative"
-- :}
-- informative at="I" loc="interactive:Ghci11:45:66"
debug :: HasCallStack => Logger -> Format (IO ()) a -> a
debug lgr = logIO lgr Debug getLoc
{-# INLINE debug #-}

-- | Like 'debug', but at 'Info'
info :: HasCallStack => Logger -> Format (IO ()) a -> a
info lgr = logIO lgr Info getLoc
{-# INLINE info #-}

-- | Like 'debug', but at 'Err'
err :: HasCallStack => Logger -> Format (IO ()) a -> a
err lgr = logIO lgr Err getLoc
{-# INLINE err #-}

-- | Convenience to log an arbitrary 'Exception', at severity 'Err', with
-- 'CallStack'
logException :: (Exception e, HasCallStack) => Logger -> e -> IO ()
logException lgr e = err lgr (fexception % " " % fcallstack) e Stack.callStack

-- | Run an action and 'logException' if it throws. The exception is rethrown.
withExceptionLogged
    :: (MonadIO m, MonadMask m, HasCallStack)
    => Logger
    -> m a
    -> m a
withExceptionLogged lgr ma =
    Safe.withException ma $ \(e :: SomeException) -> liftIO $ logException lgr e

-- MonadReader -----------------------------------------------------------------

logM :: MonadLogger r m
     => Severity
     -> Maybe SrcLoc
     -> Format (m ()) a
     -> a
logM sev loc fmt = F.runFormat fmt $ \bldr -> do
    Logger{..} <- asks getter
    if sev < _logLvl then
        pure ()
    else
        liftIO . _logOut . FL.toLogStr $
            F.format (fmsg sev loc _logNs F.builder) bldr

-- | Log at 'Debug' in a transformer stack which has access to a 'Logger'. The
-- 'SrcLoc' of the call site is automatically added to the log statement.
--
-- >>> :{
-- withStdLogger defaultConfig { cfgLevel = Debug } . runReaderT $ do
--     infoM "oh la la"
--     debugM "shhh
-- :}
-- oh la la at="I" loc="interactive:Ghci10:43:65"
-- shhh at="D" loc="interactive:Ghci10:43:85"
debugM :: (MonadLogger r m, HasCallStack) => Format (m ()) a -> a
debugM = logM Debug getLoc
{-# INLINE debugM #-}

-- | Like 'debugM', but at 'Info'
infoM :: (MonadLogger r m, HasCallStack) => Format (m ()) a -> a
infoM = logM Info getLoc
{-# INLINE infoM #-}

-- | Like 'debugM', but at 'Err'
errM :: (MonadLogger r m, HasCallStack) => Format (m ()) a -> a
errM = logM Err getLoc
{-# INLINE errM #-}

-- | Adjust the log level for the duration of the supplied action.
--
-- >>> withStdLogger defaultConfig . runReaderT . withLevel Err $ infoM "not printed"
withLevel :: (Has Logger r, MonadReader r m) => Severity -> m a -> m a
withLevel lvl = local (set (hasLens . logLevel) lvl)

-- | Adjust the 'Namespace' for the duration of the supplied action.
--
-- >>> withStdLogger defaultConfig . runReaderT . withNamespace "somelib" $ infoM "lib logs this"
-- somelib logs this at="I" ns="somelib" loc="interactive:Ghci1:65:70"
withNamespace :: (Has Logger r, MonadReader r m) => Namespace -> m a -> m a
withNamespace ns = local (set (hasLens . logNamespace) (Just ns))

-- | Turn off logging for the supplied action.
withoutLogging :: (Has Logger r, MonadReader r m) => m a -> m a
withoutLogging = local (set hasLens noLogger)

-- | Like 'logException', but in a transformer stack which has access to a
-- 'Logger'.
logExceptionM
    :: (Exception e, HasCallStack, MonadLogger r m)
    => e
    -> m ()
logExceptionM e = asks getter >>= liftIO . (`logException` e)

-- | Like 'withExceptionLogged', but in a transformer stack which has access to
-- a 'Logger'.
withExceptionLoggedM
    :: (MonadLogger r m, MonadMask m, HasCallStack)
    => m a
    -> m a
withExceptionLoggedM ma = asks getter >>= (`withExceptionLogged` ma)

-- Pure (MonadWriter) ----------------------------------------------------------

data LogRecord = LogRecord Severity (Maybe SrcLoc) LTB.Builder

type Logs = DList LogRecord

logP :: MonadWriter Logs m
     => Severity
     -> Maybe SrcLoc
     -> Format (m ()) a
     -> a
logP sev loc fmt = F.runFormat fmt $ tell . singleton . LogRecord sev loc

-- | Log at 'Debug' in a pure (non-IO) monad.
--
-- The accumulated log statements can be output in 'IO' using 'flush' or
-- 'flushM'.
--
-- >>> let (_,logs) = runWriter $ infoP "a" *> infoP "b" in withStdLogger defaultConfig (`flush` logs)
-- a at="I" loc="interactive:Ghci1:19:28"
-- b at="I" loc="interactive:Ghci1:19:41"
debugP :: (MonadWriter Logs m, HasCallStack) => Format (m ()) a -> a
debugP = logP Debug getLoc
{-# INLINE debugP #-}

-- | Like 'debugP', but a 'Info'
infoP :: (MonadWriter Logs m, HasCallStack) => Format (m ()) a -> a
infoP = logP Info getLoc
{-# INLINE infoP #-}

-- | Like 'debugP', but at 'Err'
errP :: (MonadWriter Logs m, HasCallStack) => Format (m ()) a -> a
errP = logP Err getLoc
{-# INLINE errP #-}

-- | Flush a sequence of 'LogRecord's to the given 'Logger's output.
flush :: Foldable t => Logger -> t LogRecord -> IO ()
flush lgr =
    traverse_ $ \(LogRecord sev loc msg) -> logIO lgr sev loc F.builder msg

-- | Like 'flush', but in a transformer stack which has access to a 'Logger'
flushM :: (MonadLogger r m, Foldable t) => t LogRecord -> m ()
flushM xs = asks getter >>= (liftIO . (`flush` xs))

-- Formatters ------------------------------------------------------------------

-- | Format an 'Exception'
--
-- This currently just calls 'show'
fexception :: Exception e => Format t (e -> t)
fexception = F.shown

-- | Format a 'CallStack' as a tag @callstack=\"the\\ncall\\nstack\"@
fcallstack :: Format t (CallStack -> t)
fcallstack = F.mapf Stack.prettyCallStack (ftag "callstack" % fquoted)

-- | Format a 'ThreadId' as a tag @thread=\"ThreadId 42\"@
fthreadId :: Format t (ThreadId -> t)
fthreadId = F.mapf show (ftag "thread" % fquoted)

-- | Format a 'ProcessID' as a tag @pid=42@
fprocessId :: Format t (ProcessID -> t)
fprocessId = F.mapf (fromIntegral @CPid @Int32) (ftag "pid" % F.int)

class ToEncoding a where
    toEncoding :: a -> Enc.Encoding

instance ToEncoding Enc.Encoding where
    toEncoding = identity
    {-# INLINE toEncoding #-}

instance ToEncoding Text where
    toEncoding = Enc.text
    {-# INLINE toEncoding #-}

instance ToEncoding LT.Text where
    toEncoding = Enc.lazyText
    {-# INLINE toEncoding #-}

instance ToEncoding String where
    toEncoding = Enc.string
    {-# INLINE toEncoding #-}

-- | Quote a textual type (one of: 'Text', 'LT.Text', or 'String') via @aeson@'s
-- escaping rules.
--
-- Most useful in conjunction with 'ftag':
--
-- >>> putStrLn $ formatToString (ftag "str" % fquoted) "string with \"quotes\"\nand newline"
-- str="string with \"quotes\"\nand newline"
fquoted :: ToEncoding e => Format t (e -> t)
fquoted =
    F.mapf (LT.decodeUtf8 . Enc.encodingToLazyByteString . toEncoding) F.text

-- | Format a tag.
--
-- >>> format (ftag "foo" % hex) 123
-- "foo=7b"
ftag :: Text -> Format r r
ftag k = F.now (F.bprint (F.stext % "=") k)

-- Internal --------------------------------------------------------------------

fsev :: Format t (Severity -> t)
fsev = flip F.mapf (ftag "at" % "\"" % F.char % "\"") $ \case
    Debug -> 'D'
    Info  -> 'I'
    Err   -> 'E'

floc :: Format t (SrcLoc -> t)
floc = ftag "loc" % "\"" % fpkg % ":" <> fmod % ":" <> fline % ":" <> fcol % "\""
  where
    fpkg  = F.mapf srcLocPackage   F.string
    fmod  = F.mapf srcLocModule    F.string
    fline = F.mapf srcLocStartLine F.int
    fcol  = F.mapf srcLocStartCol  F.int

fmay :: Format LTB.Builder (a -> LTB.Builder) -> Format t (Maybe a -> t)
fmay fmt = F.later (maybe mempty (F.bprint fmt))

fns :: Format t (Namespace -> t)
fns = F.mapf fromNamespace (ftag "ns" % fquoted)

fmsg :: Severity -> Maybe SrcLoc -> Maybe Namespace -> Format t a -> Format t a
fmsg sev loc ns fmt =
      fmt
    % " " % now' fsev sev
    % now' (fmay (" " % fns)) ns
    % " " % now' (fmay floc) loc
  where
    now' f = F.now . F.bprint f

getLoc :: HasCallStack => Maybe SrcLoc
getLoc = case Stack.getCallStack Stack.callStack of
    [] -> Nothing
    xs -> Just . snd $ last xs
