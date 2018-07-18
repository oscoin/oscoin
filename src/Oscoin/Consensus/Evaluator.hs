module Oscoin.Consensus.Evaluator where

import           Oscoin.Prelude

import           Control.Monad.Except
import           Control.Monad.State

newtype EvalError = EvalError { fromEvalError :: Text }
    deriving (Eq, Show, Read, Semigroup, Monoid, IsString)

-- | The monad in which transaction languages run.
newtype LanguageM s a = LanguageM
    { fromLanguageM :: ExceptT EvalError (State s) a }
    deriving (Functor, Applicative, Monad, MonadState s, MonadError EvalError)

type Evaluator s a b = a -> s -> Maybe (b, s)

-- | An evaluator that accepts any expression and has no state.
acceptAnythingEval :: Evaluator s a ()
acceptAnythingEval _ s = Just ((), s)

-- | An evaluator that rejects any expression and has no state.
rejectEverythingEval :: Evaluator s a b
rejectEverythingEval _ = const Nothing

-- TODO(alexis): Reimplement in terms of applyValidExprs.
evals :: Foldable t => t a -> s -> Evaluator s a b -> Maybe s
evals exprs st eval =
    go (toList exprs) st
  where
    go [] s = Just s
    go (expr:es) s =
        case eval expr s of
            Just (_, s') -> go es s'
            Nothing      -> Nothing

-- | Given a bunch of expressions, runs those that don't result in an error in
-- order.
applyValidExprs
    :: Foldable t
    => t a                             -- ^ The expressions to evaluate.
    -> s                               -- ^ The initial state.
    -> Evaluator s a b                 -- ^ The evaluation funcion.
    -> ([Either EvalError a], s)       -- ^ A list of results and maybe a new state @s@.
applyValidExprs exprs st eval =
    -- TODO: Do we want to return the 'b's?
    go [] (toList exprs) st
  where
    go acc (expr:es) s =
        case eval expr s of
            Just (_, s') -> -- Successful evaluation.
                go (Right expr : acc) es s'
            Nothing      -> -- Failed evaluation.
                go (Left (EvalError "Evaluation failed") : acc) es s
    go acc [] s = (reverse acc, s)
