{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

-- code-review: ignore
-- Temporarily inlined from unreleased library

-- Copyright   : 2019 Kim Altintop
-- License     : BSD3
-- Maintainer  : kim.altintop@gmail.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module System.Console.Option
    ( Cmd     (..)
    , SomeCmd (..)
    , Opt     (..)
    , SomeOpt (..)
    , Var
    , Sh      (..)
    , Op      (..)

    , Eval
    , EvalError
    , runEval

    , evalCmd
    , evalOpt
    , evalSomeOpt
    , evalSh
    , evalShInt
    , evalArith
    )
where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List (intersperse)
import           Data.Proxy (Proxy(..))
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Build
import qualified Data.Text.Lazy.Builder.Int as Build
import qualified Data.Text.Read as Read
import           Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import           Formatting.Buildable (Buildable(..))
import           Prelude


data Cmd a = Cmd FilePath [Opt a]
    deriving (Eq, Ord, Show, Read, Functor)

instance Buildable a => Buildable (Cmd a) where
    build (Cmd exe opts) =
        mconcat . intersperse " " $ Build.fromString exe : map build opts

data SomeCmd = SomeCmd FilePath [SomeOpt]
    deriving Show

instance Buildable SomeCmd where
    build (SomeCmd exe opts) =
        mconcat . intersperse " " $ Build.fromString exe : map build opts


data Opt a where
    Flag :: Text      -> Opt a
    Arg  :: Text      -> Opt a
    Opt  :: Text -> a -> Opt a

deriving instance Eq   a => Eq   (Opt a)
deriving instance Ord  a => Ord  (Opt a)
deriving instance Show a => Show (Opt a)
deriving instance Read a => Read (Opt a)

deriving instance Functor Opt

instance Buildable a => Buildable (Opt a) where
    build = \case
        Flag f   -> "--" <> Build.fromText f
        Opt  n v -> "--" <> Build.fromText n <> "=" <> build v
        Arg  a   -> Build.fromText a


data SomeOpt where
    ShOpt  :: (Buildable a, Show a) => Opt (Sh a) -> SomeOpt
    LitOpt ::                          Opt Text   -> SomeOpt

deriving instance Show SomeOpt

instance Buildable SomeOpt where
    build = \case
        ShOpt  opt -> build opt
        LitOpt opt -> build opt


newtype Var = Var Text
    deriving (Eq, Ord, Show, Read)

instance IsString Var where
    fromString = Var . fromString

instance Buildable Var where
    build (Var x) = Build.fromText x


data Sh a where
    ShNum  :: Int  -> Sh Int
    ShStr  :: Text -> Sh Text
    ShVar  :: Var  -> Sh Var
    ShEval :: Sh a -> Sh (Sh a)

    ShCmd  :: Typeable a => Cmd a -> Sh (Cmd a)

    ShArith
        :: ( Show      a
           , Show      b
           , Buildable a
           , Buildable b
           )
        => Op
        -> Sh a
        -> Sh b
        -> Sh Int

deriving instance Show a => Show (Sh a)

instance Buildable a => Buildable (Sh a) where
    build = \case
        ShNum   x      -> Build.decimal x
        ShStr   x      -> Build.fromText x
        ShVar   x      -> "${" <> build x <> "}"
        ShCmd   cmd    -> build cmd
        ShEval  sh     -> "$(" <> build sh <> ")"
        ShArith op a b -> "$((" <> build a <> build op <> build b <> "))"


data Op = Add | Sub | Mul | Div
    deriving (Eq, Ord, Enum, Show, Read)

instance IsString Op where
    fromString = \case
        "+" -> Add
        "-" -> Sub
        "*" -> Mul
        "/" -> Div
        x   -> error $ "Unkown arithmetic operator: " <> x

instance Buildable Op where
    build = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"


data EvalEnv = EvalEnv
    { eeVars :: Var -> Maybe Text
    , eeCmds :: forall a. Cmd a -> Maybe Text
    }

data EvalError
    = UnsetVariable  Var
    | UnknownCommand TypeRep
    | CastError      TypeRep TypeRep
    deriving Show

type Eval a = ExceptT EvalError (Reader EvalEnv) a

runEval
    :: (Var -> Maybe Text)
    -> (forall x. Cmd x -> Maybe Text)
    -> Eval a
    -> Either EvalError a
runEval vs cs = flip runReader (EvalEnv vs cs) . runExceptT

evalCmd :: Cmd (Sh a) -> Eval (Cmd Text)
evalCmd (Cmd exe opts) = Cmd exe <$> traverse evalOpt opts

evalOpt :: Opt (Sh a) -> Eval (Opt Text)
evalOpt = \case
    Flag x   -> pure $ Flag x
    Arg  x   -> pure $ Arg  x
    Opt  k v -> evalSh v >>= \case
        ShStr x -> pure $ Opt k x

evalSomeOpt :: SomeOpt -> Eval (Opt Text)
evalSomeOpt = \case
    ShOpt  x -> evalOpt x
    LitOpt x -> pure x

evalSh :: Sh a -> Eval (Sh Text)
evalSh = \case
    ShVar   x      -> ShStr <$> lookupVar x
    ShCmd   x      -> ShStr <$> runCmd    x
    ShEval  x      -> evalSh x
    ShArith op a b ->
        evalSh . ShNum =<<
            evalArith op <$> evalShInt a <*> evalShInt b
    ShNum   x      -> pure . ShStr $ Text.pack (show x)
    ShStr   x      -> pure $ ShStr x
  where
    lookupVar :: Var -> Eval Text
    lookupVar x = do
        vars <- asks eeVars
        note (UnsetVariable x) $ vars x

    runCmd :: Typeable x => Cmd x -> Eval Text
    runCmd x = do
        cmds <- asks eeCmds
        note (UnknownCommand (typeOf x)) $ cmds x

evalShInt :: Sh a -> Eval Int
evalShInt sh = evalSh sh >>= cast >>= int
  where
    int :: Sh Int -> Eval Int
    int = \case
        ShNum   x      -> pure x
        ShArith op a b -> evalArith op <$> evalShInt a <*> evalShInt b

    cast :: Sh Text -> Eval (Sh Int)
    cast (ShStr x) =
        case Read.signed Read.decimal x of
            Right (i, "") -> pure $ ShNum i
            _             -> throwError $ CastError tyFrom tyTo
      where
        tyFrom = typeRep (Proxy @(Sh Text))
        tyTo   = typeRep (Proxy @Int)

evalArith :: Op -> Int -> Int -> Int
evalArith Add = (+)
evalArith Sub = (-)
evalArith Mul = (*)
evalArith Div = div

--------------------------------------------------------------------------------

note :: MonadError e m => e -> Maybe a -> m a
note _ (Just a) = pure a
note e Nothing  = throwError e
