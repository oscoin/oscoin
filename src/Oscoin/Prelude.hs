{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- | Custom Prelude based on <https://hackage.haskell.org/package/protolude Protolude>.
--
-- Deviations from "Protolude":
--
-- * CPP'ed versions of functions for compatibility with older 'base' versions
--   have been removed.
-- * "Control.Exception.Safe" instead of "Control.Exception".
--
--     * hiding 'assert' and 'handle', as those symbols are commonly used in our
--       codebase.
--
-- * Only 'Generic' from "GHC.Generics" is exported.
-- * Additional exports:
--
--     * 'String'
--     * 'MonadTrans'
--
-- * Not exported:
--
--     * "Data.Typeable"
--     * "Data.Type.Coercion"
--     * "Data.Type.Equality"
--     * "Data.Complex"
--
-- * Export 'Prelude.error' instead of 'Protolude.Error.error'. The
--   latter has a bug that swallows stack traces. See
--   https://github.com/sdiehl/protolude/pull/102.
--
-- * 'undefined' and 'notImplemented' have 'HasCallStack' constraint
--   and use 'Prelude.error'.
--
-- To see the full list of exported symbols, run
--
-- @
-- Oscoin.Prelude> :browse!
-- @
--
-- in GHCi.
module Oscoin.Prelude
    ( module Base
    , module X
    , module Control.Exception.Safe

    , Actual
    , Expected
    , String
    , LText
    , LByteString

    , applyN
    , die
    , error
    , guarded
    , guardedA
    , identity
    , liftIO1
    , liftIO2
    , map
    , notImplemented
    , pass
    , print
    , show
    , uncons
    , undefined
    , unsnoc
    ) where

import           Control.Exception.Safe hiding (assert, handle)

-- 'Protolude.Error.error' does not add stack traces to the error.
-- Switch to 'Protolude.Error.error' again when
-- https://github.com/sdiehl/protolude/pull/102 is released.
import           Prelude (String, error)

import           Data.String as X (IsString)

-- We define alternative versions of 'notImplemented' and 'undefined'
-- that have the 'HasCallStack' constraint.
import           Debug as X hiding (notImplemented, undefined)

import           Protolude.Base as Base hiding
                 ( print
                 , putStr
                 , putStrLn
                 , show
                 , showFloat
                 , showList
                 , showSigned
                 , showSignedFloat
                 , showsPrec
                 , (%)
                 )
import qualified Protolude.Base as PBase

import           Protolude.Applicative as X
import           Protolude.Bool as X
import           Protolude.Conv as X
import           Protolude.Either as X
import           Protolude.Exceptions as X hiding (tryIO)
import           Protolude.Functor as X
import           Protolude.List as X
import           Protolude.Monad as X
import           Protolude.Panic as X
import           Protolude.Semiring as X
import           Protolude.Show as X

-- Maybe'ized version of partial functions
import           Protolude.Safe as X
                 ( atDef
                 , atMay
                 , foldl1May
                 , foldl1May'
                 , foldr1May
                 , headDef
                 , headMay
                 , initDef
                 , initMay
                 , initSafe
                 , lastDef
                 , lastMay
                 , maximumDef
                 , maximumMay
                 , minimumDef
                 , minimumMay
                 , tailDef
                 , tailMay
                 , tailSafe
                 )

-- Applicatives
import           Control.Applicative as X
                 ( Alternative(..)
                 , Applicative(..)
                 , Const(..)
                 , ZipList(..)
                 , liftA
                 , liftA2
                 , liftA3
                 , optional
                 , (<**>)
                 )


-- Base typeclasses
import           Data.Eq as X (Eq(..))
import           Data.Foldable as X hiding (foldl1, foldr1, product, sum)
import           Data.Functor.Identity as X (Identity(..))
import           Data.Ord as X (Down(..), Ord(..), Ordering(..), comparing)
import           Data.Traversable as X

import           Data.List.NonEmpty as X (NonEmpty(..), nonEmpty)
import           Data.Semigroup as X
                 ( Option(..)
                 , Semigroup(sconcat, stimes)
                 , WrappedMonoid
                 , cycle1
                 , diff
                 , mtimesDefault
                 , option
                 , stimesIdempotent
                 , stimesIdempotentMonoid
                 , stimesMonoid
                 )

import           Data.Monoid as X

import           Data.Bifunctor as X (Bifunctor(..))

-- Deepseq
import           Control.DeepSeq as X (NFData(..), deepseq, force, ($!!))

-- Data structures
import           Data.Tuple as X (curry, fst, snd, swap, uncurry)

import           Data.List as X
                 ( break
                 , cycle
                 , drop
                 , dropWhile
                 , filter
                 , genericDrop
                 , genericLength
                 , genericReplicate
                 , genericSplitAt
                 , genericTake
                 , group
                 , inits
                 , intercalate
                 , intersperse
                 , isPrefixOf
                 , iterate
                 , permutations
                 , repeat
                 , replicate
                 , reverse
                 , scanl
                 , scanl'
                 , scanr
                 , sort
                 , sortBy
                 , splitAt
                 , subsequences
                 , tails
                 , take
                 , takeWhile
                 , transpose
                 , unfoldr
                 , unzip
                 , zip
                 , zipWith
                 )

import           Data.IntMap as X (IntMap)
import           Data.IntSet as X (IntSet)
import           Data.Map as X (Map)
import           Data.Sequence as X (Seq)
import           Data.Set as X (Set)

import           Data.Proxy as X (Proxy(..))

import           Data.Void as X (Void, absurd, vacuous)

-- Monad transformers
import           Control.Monad.State as X
                 ( MonadState
                 , State
                 , StateT(StateT)
                 , evalState
                 , evalStateT
                 , execState
                 , execStateT
                 , get
                 , gets
                 , modify
                 , put
                 , runState
                 , runStateT
                 , state
                 , withState
                 )

import           Control.Monad.Reader as X
                 ( MonadReader
                 , Reader
                 , ReaderT(ReaderT)
                 , ask
                 , asks
                 , local
                 , reader
                 , runReader
                 , runReaderT
                 )

import           Control.Monad.Trans.Except as X (catchE, throwE)

import           Control.Monad.Except as X
                 ( Except
                 , ExceptT(ExceptT)
                 , MonadError
                 , catchError
                 , mapExcept
                 , mapExceptT
                 , runExcept
                 , runExceptT
                 , throwError
                 , withExcept
                 , withExceptT
                 )

import           Control.Monad.Trans as X (MonadIO, MonadTrans, lift, liftIO)

-- Base types
import           Data.Bits as X hiding (unsafeShiftL, unsafeShiftR)
import           Data.Int as X (Int, Int16, Int32, Int64, Int8)
import           Data.Word as X
                 ( Word
                 , Word16
                 , Word32
                 , Word64
                 , Word8
                 , byteSwap16
                 , byteSwap32
                 , byteSwap64
                 )

import           Data.Either as X
                 ( Either(..)
                 , either
                 , isLeft
                 , isRight
                 , lefts
                 , partitionEithers
                 , rights
                 )

import           Data.Bool as X hiding (bool)
import           Data.Char as X (chr)
import           Data.Maybe as X hiding (fromJust)

import           Data.Function as X (const, fix, flip, on, ($), (&), (.))

-- Generics
import           GHC.Generics as X (Generic)

-- ByteString
import           Data.ByteString as X (ByteString)
import qualified Data.ByteString.Lazy

-- Text
import           Data.Text as X (Text)
import qualified Data.Text.Lazy

import           Data.Text.IO as X
                 ( appendFile
                 , getContents
                 , getLine
                 , interact
                 , readFile
                 , writeFile
                 )

import           Data.Text.Encoding as X
                 (decodeUtf8, decodeUtf8', decodeUtf8With, encodeUtf8)

import           Data.Text.Encoding.Error as X
                 ( OnDecodeError
                 , OnError
                 , UnicodeException
                 , ignore
                 , lenientDecode
                 , replace
                 , strictDecode
                 )

-- IO
import           System.Environment as X (getArgs)
import           System.Exit as X
                 (ExitCode(..), exitFailure, exitSuccess, exitWith)
import qualified System.Exit
import           System.IO as X
                 ( FilePath
                 , IOMode(..)
                 , openFile
                 , stderr
                 , stdin
                 , stdout
                 , withFile
                 )

-- ST
import           Control.Monad.ST as X (ST, fixST, runST)


import           Control.Concurrent as X hiding (throwTo, yield)
import           Control.Concurrent.Async as X
                 ( Async(..)
                 , Concurrently(..)
                 , async
                 , asyncBound
                 , asyncOn
                 , asyncThreadId
                 , cancel
                 , cancelWith
                 , concurrently
                 , link
                 , link2
                 , poll
                 , race
                 , race_
                 , wait
                 , waitAny
                 , waitAnyCancel
                 , waitAnyCatch
                 , waitAnyCatchCancel
                 , waitBoth
                 , waitCatch
                 , waitEither
                 , waitEitherCancel
                 , waitEitherCatch
                 , waitEitherCatchCancel
                 , waitEither_
                 , withAsync
                 , withAsyncBound
                 , withAsyncOn
                 )
import           Control.Monad.STM as X
                 (STM, atomically, catchSTM, check, orElse, retry, throwSTM)

import           Foreign.Ptr as X (IntPtr, WordPtr)
import           Foreign.StablePtr as X (StablePtr)
import           Foreign.Storable as X (Storable)

-- Read instances hiding unsafe builtins (read)
import           Text.Read as X (Read, readEither, readMaybe, reads)

{-# ANN module ("HLint: ignore Avoid restricted qualification" :: String ) #-}

-- Type synonymss for lazy texts
type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString


identity :: a -> a
identity x = x

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

unsnoc :: [x] -> Maybe ([x],x)
unsnoc = foldr go Nothing
  where
    go x mxs = Just (case mxs of
       Nothing      -> ([], x)
       Just (xs, e) -> (x:xs, e))

applyN :: Int -> (a -> a) -> a -> a
applyN n f = X.foldr (.) identity (X.replicate n f)

print :: (X.MonadIO m, PBase.Show a) => a -> m ()
print = liftIO . PBase.print

-- | Do nothing returning unit inside applicative.
pass :: Applicative f => f ()
pass = pure ()

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = X.bool empty (pure x) (p x)

guardedA :: (Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
guardedA p x = X.bool empty (pure x) <$> p x

-- | Lift an 'IO' operation with 1 argument into another monad
liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO

-- | Lift an 'IO' operation with 2 arguments into another monad
liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 = ((.).(.)) liftIO

show :: (Show a, StringConv String b) => a -> b
show x = toS (PBase.show x)
{-# SPECIALIZE show :: Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show  a => a -> ByteString  #-}
{-# SPECIALIZE show :: Show  a => a -> LByteString  #-}
{-# SPECIALIZE show :: Show  a => a -> String  #-}

die :: Text -> IO a
die err = System.Exit.die (toS err)


{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: (HasCallStack) => a
notImplemented = withFrozenCallStack (error "Not implemented")

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: (HasCallStack) => a
undefined = withFrozenCallStack (error "Prelude.undefined")

-- Simple type aliases that makes easier for the reader to distinguish, in
-- a type signature, which is the expected vs actual value.
type Expected a = a
type Actual   a = a
