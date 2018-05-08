{-# LANGUAGE NoImplicitPrelude #-}
module Oscoin.Prelude
    ( module Prelude
    , module Control.Applicative
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Writer.CPS
    , module Control.Monad.Trans.Class
    , module Control.Monad.STM.Class
    , module Control.Concurrent.STM
    , module Control.Monad.Fail
    , module Control.Monad
    , module Data.Semigroup
    , module Data.Traversable
    , module Data.Foldable
    , module Data.Functor.Identity
    , module Data.Text
    , module Data.ByteString
    , module Data.Map
    , module Data.IORef
    , module Data.Sequence
    , module Data.List.NonEmpty
    , module Data.Has
    , module Data.Either
    , module Data.Default
    , module Data.ByteArray
    , module Data.Function
    , module Data.Maybe
    , module Data.Functor
    , module Data.String
    , module Data.Word
    , module Data.Set
    , module GHC.Stack
    , module GHC.Generics
    , module Debug.Trace
    , MonadIO
    , LByteString
    , Error(..)
    , Timestamp
    , Id
    , notImplemented
    , io
    , pass
    , read
    , readStr
    , readIO
    , readFile
    , (<&>)
    , map
    , foreach
    , encodeUtf8
    , decodeUtf8
    , tshow
    , (++)
    , concat
    , intercalate
    , sum
    , product
    , identity
    ) where

import           Prelude hiding ( fail, read, readIO, readFile
                                , (++), concat, sum, product, id, map )
import qualified Prelude
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Semigroup (Semigroup, (<>))
import           Data.IORef (IORef)
import           Data.Traversable (Traversable(..), sequence, traverse)
import           Data.Foldable (for_, traverse_, Foldable, toList, null, foldr, foldl')
import           Data.Sequence (Seq)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List
import           Data.Has
import           Data.Either (isRight, isLeft)
import           Data.Default (def, Default)
import           Data.ByteArray (ByteArrayAccess)
import           Data.Function ((&))
import           Data.Functor (void)
import           Data.Functor.Identity (Identity)
import           Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import           Data.String (IsString)
import           Data.Word
import           Control.Applicative (liftA2)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Reader (Reader, MonadReader, ReaderT(..), runReaderT, ask, asks, local)
import           Control.Monad.State (MonadState, runState, runStateT, execStateT, evalStateT)
import           Control.Monad.Writer.CPS (MonadWriter, runWriter, runWriterT, execWriter, tell)
import           Control.Monad.STM.Class (MonadSTM, liftSTM)
import           Control.Monad (forM, mapM)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Monad.Fail (MonadFail, fail)
import           GHC.Stack (HasCallStack)
import           GHC.Generics (Generic)
import           Debug.Trace (trace, traceShow, traceM, traceShowM, traceShowId)

type LByteString = LBS.ByteString

newtype Error = Error { fromError :: Text }
    deriving (Show, Eq, IsString)

-- | Unix timestamp.
type Timestamp = Word32

-- | 'Id' maps a type to its identifier type. Example: 'Hashed' @tx@ for @tx@.
type family Id a :: *

notImplemented :: HasCallStack => a
notImplemented = error "Not implemented"

-- | > io = liftIO
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Do nothing and return unit inside applicative.
pass :: Applicative f => f ()
pass = pure ()

-- | Parse Text to a value.
read :: Read a => Text -> a
read = Prelude.read . T.unpack

-- | Parse String to a value.
readStr :: Read a => String -> a
readStr = Prelude.read

-- | The readIO function is similar to read
-- except that it signals parse failure to the IO monad
-- instead of terminating the program.
readIO :: Read a => Text -> IO a
readIO = Prelude.readIO . T.unpack

-- | Read a file and return the contents of the file as Text.
-- The entire file is read strictly.
readFile :: FilePath -> IO Text
readFile = T.readFile

-- | Note that this is /not/ the standard @Data.Text.Encoding.decodeUtf8@. That
-- function will throw impure exceptions on any decoding errors. This function
-- instead uses @decodeLenient@.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

-- | Convert a value to readable Text.
tshow :: Show a => a -> Text
tshow = T.pack . Prelude.show

-- | Infix version of foreach.
--
-- @<&>@ is to '<$>' what '&' is to '$'.
infixl 4 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = foreach

-- | > map = fmap
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

-- | > foreach = flip fmap
foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip map

-- | > (++) = mappend
infixr 5 ++
(++) :: Monoid w => w -> w -> w
(++) = mappend

-- | > concat = mconcat
concat :: Monoid w => [w] -> w
concat = mconcat

-- | > intercalate = mconcat .: intersperse
intercalate :: Monoid w => w -> [w] -> w
intercalate xs xss = concat (Data.List.intersperse xs xss)

-- | Compute the sum of a finite list of numbers.
sum :: (Foldable f, Num a) => f a -> a
sum = Data.Foldable.foldl' (+) 0

-- | Compute the product of a finite list of numbers.
product :: (Foldable f, Num a) => f a -> a
product = Data.Foldable.foldl' (*) 1

-- | The identity function.
identity :: a -> a
identity = Prelude.id
