module Oscoin.Prototype.V2 where

import           Oscoin.Prelude

import           Control.Applicative
import           Control.Concurrent (Chan, readChan)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Char (isAlphaNum, isLetter)
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Text.Megaparsec (Parsec, between, choice, manyTill, sepBy1)
import           Text.Megaparsec.Char (char, satisfy, space1)
import qualified Text.Megaparsec.Char.Lexer as L

------------------------------------------------------------------------------
-- Language

data LangError =
      UnknownIdentifier Ident
    | Impossible Text
    | TypeError Text
    | WrongNumberOfArgs
          Text -- ^ function name
          Int    -- ^ expected
          Int    -- ^ actual
    | OtherError Text
    deriving (Eq, Show, Read, Generic)

newtype Ident = Ident { fromIdent :: Text }
    deriving (Eq, Show, Read, Ord, Generic, IsString)

data Value =
      Atom Ident
    | String Text
    | Boolean Bool
    | List [Value]
    | Primop Ident
    | Apply Value [Value]
    | SortedMap (Map.Map Value Value)
    | Lambda
        [Ident] -- ^ parameters
        [Value] -- ^ body
        Env     -- ^ closure
    | If Value Value Value
    deriving (Eq, Ord, Show, Read, Generic)

data Top
    = Define Ident Value
    deriving (Eq, Ord, Show, Read, Generic)

newtype Env = Env { fromEnv :: Map Ident Value }
    deriving (Eq, Show, Ord, Read, Semigroup, Monoid, Generic)

newtype LangM a = LangM
    { fromLangM :: ExceptT LangError (State Env) a }
    deriving (Functor, Applicative, Monad, MonadError LangError, MonadState Env)

runLangM :: Env -> LangM a -> (Either LangError a, Env)
runLangM e l = runState (runExceptT $ fromLangM l) e

-- | Lookup an atom in the environment
lookupAtom :: Ident -> LangM Value
lookupAtom i = get >>= \(Env env) -> case Map.lookup i env of
    Nothing -> throwError $ UnknownIdentifier i
    Just v  -> pure v

-- | Lookup a primop.
lookupPrimop :: Ident -> LangM ([Value] -> LangM Value)
lookupPrimop i = case Map.lookup i primops of
    Nothing -> throwError $ Impossible "Unknown primop"
    Just v  -> pure v

primops :: Map Ident ([Value] -> LangM Value)
primops = Map.fromList
    [ ("sorted-map", \args -> do
          unless (even $ length args) $ throwError $
              OtherError "'sorted-map' requires an even number of arguments"
          pure $ SortedMap $ Map.fromList (zip args $ tail args))
    , ("eval", \args -> case args of
          [List (v:vs)] -> eval (Apply v vs)
          _             -> throwError $ OtherError "'eval' requires list as argument")
    , ("string?", \args -> case args of
          [String _] -> pure $ Boolean True
          [_]        -> pure $ Boolean False
          xs         -> throwError $ WrongNumberOfArgs "string?" 1 (length xs))
    , ("boolean?", \args -> case args of
          [Boolean _] -> pure $ Boolean True
          [_]         -> pure $ Boolean False
          xs          -> throwError $ WrongNumberOfArgs "boolean?" 1 (length xs))
    , ("begin", \args -> case args of
          -- Whether (begin) with no arguments is allowed seems to vary by
          -- implementation. Rather than returning an arbitrary value, we just
          -- disallow it.
          _:_ -> last <$> traverse eval args
          _   -> throwError $ OtherError "begin needs at least one argument")
    ]

-- ** Eval

-- | Note that evaluation is still not appropriately lazy for special
-- forms/primops. (E.g., (if cond t-exp f-exp) evaluates t-exp and f-exp.)
eval :: Value -> LangM Value
eval val = case val of
    Atom i -> lookupAtom i
    List vals -> List <$> traverse eval vals
    String s -> pure $ String s
    Boolean b -> pure $ Boolean b
    Apply mfn vs -> do
        mfn' <- eval mfn
        vs' <- traverse eval vs
        case mfn' of
            Primop i -> do
                fn <- lookupPrimop i
                fn vs'
            Lambda bnds body (Env env) ->
                if | length bnds /= length vs' -> throwError $
                          WrongNumberOfArgs "lambda" (length bnds) (length vs')
                   | null body -> throwError $
                          OtherError "bad syntax (no expressions in body)"
                   | otherwise -> do
                      let mappings = Map.fromList (zip bnds vs')
                      oldState <- get
                      put . Env $ Map.union mappings env
                      res <- last <$> traverse eval body
                      put oldState
                      pure res
            _ -> throwError $ TypeError "Trying to apply a non-function"
    Primop i -> pure $ Primop i
    e@(Lambda _ _ _) -> pure e
    SortedMap mp -> do
        let bieval (a,b) = (,) <$> eval a <*> eval b
        SortedMap . Map.fromList <$> traverse bieval (Map.toList mp)
    If cond t f -> do
        b <- eval cond
        -- I hate this as much as everyone that might ever read Haskell, but
        -- in Lisps a lot of things that one might object to are True...
        if b == Boolean False then eval f else eval t

evalTop :: Top -> LangM Value
evalTop t = case t of
    Define i v -> do
        r <- eval v
        modify (\(Env e) -> Env $ Map.insert i r e)
        pure $ List [Atom "Define", Atom i]

-- ** Helpers

-- | Add a definition. Should ultimately check that identifier isn't already
-- being used.
define :: Ident -> Value -> LangM Value
define i v = evalTop (Define i v)

-- | Lambdas. Captures the environment at the definition site in a closure.
lambda :: [Ident] -> [Value] -> LangM Value
lambda ix body = Lambda ix body <$> get

-- | Infix function application
infixr 1 $$
($$) :: Value -> [Value] -> Value
($$) = Apply

-- ** Parsing

type Parser = Parsec Text Text

spaceConsumer :: Parser () -- Parsec e s ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment ";;"
    blockComment = L.skipBlockComment "#|" "|#" -- R6RS


symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parensP :: Parser a -> Parser a
parensP = between (symbol "(") (symbol ")")

primopP :: Parser Value
primopP = Primop . Ident <$> (choice $ symbol . fromIdent <$> Map.keys primops)

stringLiteralP :: Parser Value
stringLiteralP = String . T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

boolLiteralP :: Parser Value
boolLiteralP = Boolean <$> (char '#' >>
        (char 't' >> pure True) <|> (char 'f' >> pure False))

atomP :: Parser Value
atomP = do
    l <- satisfy (\x -> isLetter x || x `elem` ext)
    r <- many (satisfy (\x -> isAlphaNum x || x `elem` ext))
    pure . Atom $ fromString (l:r)
  where
    ext = ['!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<' , '=', '>'
          , '?', '@', '^', '_', '~']

applyP :: Parser Value
applyP = parensP $ do
    fn:args <- valueP `sepBy1` spaceConsumer
    pure $ fn $$ args

valueP :: Parser Value
valueP = choice
    [ stringLiteralP
    , boolLiteralP
    , primopP
    , atomP
    , applyP
    ]

------------------------------------------------------------------------------
-- Chains

-- | A blockchain definition.
data Chain m a = Chain
    { chainName     :: [Text]
    -- | The fold function. The Value returned is a hint to callbacks, and does
    -- not ever get fed back into chainStep.
    , chainStep     :: Env -> Top -> Either LangError (Value, Env)
    , chainEnv      :: Env
    -- | 'chainCallback' is called with each new Env and with the hint given by
    -- the chainStep. A Left means evaluation failed.
    , chainCallback :: Either LangError (Value, Env) -> m a
    } deriving (Functor, Generic)

genesisChain :: Chain Identity (Either LangError (Value, Env))
genesisChain = Chain
    { chainName = []
    , chainEnv = mempty
    , chainStep = \env top -> case runLangM env (evalTop top) of
        (Left e, _)     -> Left e
        (Right v, env') -> Right (v, env')
    , chainCallback = pure
    }

-- A simple chain runner that takes new values from the provided Chan.
-- Runs until chainStep returns a Left.
runChainFromChannel :: Chan Top -> Chain IO a -> IO a
runChainFromChannel chan chain = do
    newTop <- readChan chan
    let mnewEnv = chainStep chain (chainEnv chain) newTop
    res <- chainCallback chain mnewEnv
    case mnewEnv of
        Left _            -> pure res
        Right (_, newEnv) -> runChainFromChannel chan $
            chain { chainEnv = newEnv }
