module Oscoin.Consensus.Evaluator
    ( Evaluator
    , EvalError(..)
    , EvalResult
    , applyValidExprs
    , evals

    , identityEval
    , foldEval
    ) where

import           Oscoin.Prelude

newtype EvalError = EvalError { fromEvalError :: Text }
    deriving (Eq, Show, Read, Semigroup, Monoid, IsString)

type EvalResult state output = Either [EvalError] (output, state)

type Evaluator state tx output = tx -> state -> EvalResult state output

-- | The identity evaluator. An evaluator that accepts any expression and has
-- no state.
identityEval :: Evaluator s a ()
identityEval _ s = Right ((), s)

foldEval :: Monoid w => Evaluator w w ()
foldEval x xs = Right ((), xs <> x)

-- | Evaluates a list of expressions with the given starting state and evaluator.
-- If any expression fails to evaluate, the function aborts and 'Nothing'
-- is returned. Otherwise, the final state is returned.
evals :: Foldable t => t a -> s -> Evaluator s a b -> Either [EvalError] s
evals exprs st eval =
    if any isLeft results then Left (concat $ lefts results) else Right st'
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
    -> ([Either [EvalError] (a, b)], s)  -- ^ A list of results and a new state @s@.
applyValidExprs exprs st eval =
    go [] (toList exprs) st
  where
    go acc (expr:es) s =
        case eval expr s of
            Right (res, s') -> -- Successful evaluation.
                go (Right (expr, res) : acc) es s'
            Left errs       -> -- Failed evaluation.
                go (Left errs : acc) es s
    go acc [] s = (reverse acc, s)
