module Oscoin.Crypto.Blockchain.Eval
    ( Receipt(..)
    , buildBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Evaluator
import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto

import           Codec.Serialise (Serialise)


-- | A 'Receipt' is generated whenever a transaction is evaluated as
-- part of a block.
data Receipt tx o = Receipt
    { receiptTx       :: Crypto.Hashed tx
    , receiptTxOutput :: Either [EvalError] o
    , receiptTxBlock  :: BlockHash
    -- ^ Identifies the block the output was generated in
    } deriving (Show, Eq, Generic)

mkReceipt :: (Crypto.Hashable tx) => Block tx s -> tx -> Either [EvalError] o -> Receipt tx o
mkReceipt block tx result = Receipt (Crypto.hash tx) result (blockHash block)


-- | Build a block by evaluating all the transactions and generating
-- receipts for them.
--
-- Only transactions that evaluate successfully are included in the
-- block but we generate receipts for all transactions.
--
-- The block header is not sealed.
buildBlock
    :: (Serialise tx, Crypto.Hashable tx)
    => Evaluator s tx o
    -> Timestamp
    -> [tx]
    -> Block tx s
    -> (Block tx s, [Receipt tx o])
buildBlock eval tick txs parent =
    let initialState = blockState $ blockHeader parent
        (txOutputs, newState) = evalTraverse eval txs initialState
        validTxs = [tx | (tx, Right _) <- txOutputs]
        newBlock = mkUnsealedBlock parent tick validTxs newState
        receipts = map (uncurry $ mkReceipt newBlock) txOutputs
    in (newBlock, receipts)

-- Internal ------------------------------------------------------

-- | Traverses transactions, evalutes them against the
-- given state and provides the output of the transaction.
--
-- If evaluation of a transaction fails the state remains untouched.
evalTraverse
    :: (Traversable t)
    => Evaluator state tx output
    -> t tx
    -> state
    -> (t (tx, Either [EvalError] output), state)
evalTraverse eval txs s = runState (traverse go txs) s
  where
    go tx = do
        result <- evalToState eval tx
        pure (tx, result)

evalToState :: Evaluator state tx output -> tx -> State state (Either [EvalError] output)
evalToState eval tx = state go
  where
    go st =
        case eval tx st of
            Left err            -> (Left err, st)
            Right (output, st') -> (Right output, st')


-- | Return an unsealed block holding the given transactions and state
-- on top of the given parent block.
mkUnsealedBlock :: (Foldable t, Serialise tx) => Block tx s -> Timestamp -> t tx -> s -> Block tx s
mkUnsealedBlock parent blockTimestamp txs blockState = mkBlock header txs
  where
    header =
         BlockHeader
            { blockPrevHash     = blockHash parent
            , blockDataHash     = hashTxs txs
            , blockState
            , blockDifficulty   = 0
            , blockTimestamp
            , blockNonce        = 0
            }
