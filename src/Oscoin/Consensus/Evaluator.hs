module Oscoin.Consensus.Evaluator
    ( Evaluator
    , EvalError
    , identityEval
    , foldEval
    , constEval
    , evals
    , applyValidExprs
    , rejectEverythingEval
    ) where

import           Oscoin.Prelude

newtype EvalError = EvalError { fromEvalError :: Text }
    deriving (Eq, Show, Read, Semigroup, Monoid, IsString)

type Evaluator s a b = a -> s -> Maybe (b, s)

-- | The identity evaluator. An evaluator that accepts any expression and has
-- no state.
identityEval :: Evaluator s a ()
identityEval _ s = Just ((), s)

-- | An evaluator that rejects any expression and has no state.
rejectEverythingEval :: Evaluator s a b
rejectEverythingEval _ = const Nothing

foldEval :: Monoid w => Evaluator w w ()
foldEval x xs = Just ((), xs <> x)

constEval :: s -> Evaluator s a ()
constEval s _ _ = Just ((), s)

-- | Evaluates a list of expressions with the given starting state and evaluator.
-- If any expression fails to evaluate, the function aborts and 'Nothing'
-- is returned. Otherwise, the final state is returned.
evals :: Foldable t => t a -> s -> Evaluator s a b -> Either [EvalError] s
evals exprs st eval =
    if any isLeft results then Left (lefts results) else Right st'
  where
    (results, st') = applyValidExprs exprs st eval

-- | Evaluate a list of expressions with the given starting state and evaluator.
-- Returns a list of evaluation results, where 'Left' means the expression
-- failed to evaluate, and 'Right' means it succeeded with a resulting value
-- of type @b@. The final state is the result of all successful expressions
-- applied in order.
applyValidExprs
    :: Foldable t
    => t a                             -- ^ The expressions to evaluate.
    -> s                               -- ^ The initial state.
    -> Evaluator s a b                 -- ^ The evaluation funcion.
    -> ([Either EvalError (a, b)], s)  -- ^ A list of results and a new state @s@.
applyValidExprs exprs st eval =
    go [] (toList exprs) st
  where
    go acc (expr:es) s =
        case eval expr s of
            Just (res, s') -> -- Successful evaluation.
                go (Right (expr, res) : acc) es s'
            Nothing        -> -- Failed evaluation.
                go (Left (EvalError "Evaluation failed") : acc) es s
    go acc [] s = (reverse acc, s)
