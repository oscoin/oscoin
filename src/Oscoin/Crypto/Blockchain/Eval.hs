{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Crypto.Blockchain.Eval
    ( Evaluator
    , EvalError(..)
    , EvalResult
    , Receipt(..)
    , buildBlock
    , evalBlock
    , evalTraverse
    ) where

import           Oscoin.Prelude


import           Oscoin.Crypto.Blockchain.Block hiding (parentHash)
import           Oscoin.Crypto.Hash (Hash)
import qualified Oscoin.Crypto.Hash as Crypto

import           Codec.Serialise (Serialise)
import qualified Crypto.Data.Auth.Tree.Class as AuthTree
import qualified Generics.SOP as SOP


type EvalResult state output = Either EvalError (output, state)

type Evaluator state tx output = tx -> state -> EvalResult state output

newtype EvalError = EvalError { fromEvalError :: Text }
    deriving (Eq, Show, Read, Semigroup, Monoid, IsString, Generic)

instance SOP.Generic EvalError
instance SOP.HasDatatypeInfo EvalError

instance Serialise EvalError

-- | A 'Receipt' is generated whenever a transaction is evaluated as
-- part of a block.
data Receipt c tx o = Receipt
    { receiptTx       :: Crypto.Hashed c tx
    , receiptTxOutput :: Either EvalError o
    , receiptTxBlock  :: BlockHash c
    -- ^ Identifies the block the output was generated in
    } deriving (Generic, Functor)

deriving instance (Show (Hash c), Show o) => Show (Receipt c tx o)
deriving instance (Eq o, Eq (Hash c)) => Eq (Receipt c tx o)

mkReceipt
    :: (Crypto.Hashable c tx)
    => Block c tx s
    -> tx
    -> Either EvalError o
    -> Receipt c tx o
mkReceipt block tx result = Receipt (Crypto.hash tx) result (blockHash block)

instance (Serialise tx, Serialise (Hash c), Serialise o) => Serialise (Receipt c tx o)


-- | Build a block by evaluating all the transactions and generating
-- receipts for them.
--
-- Only transactions that evaluate successfully are included in the
-- block but we generate receipts for all transactions.
--
-- The block header is not sealed.
buildBlock
    :: ( Serialise tx
       , Serialise (Beneficiary c)
       , Crypto.Hashable c tx
       , Crypto.Hashable c st
       , Crypto.Hashable c (BlockHeader c Unsealed)
       , AuthTree.MerkleHash (Hash c)
       )
    => Evaluator st tx o
    -> Timestamp
    -> Beneficiary c
    -> st
    -> [tx]
    -> Block c tx s
    -> (Block c tx Unsealed, st, [Receipt c tx o])
buildBlock eval tick benef st txs parentBlock =
    let initialState = st
        (txOutputs, newState) = evalTraverse eval txs initialState
        validTxs = [tx | (tx, Right _) <- txOutputs]
        newBlock = mkUnsealedBlock (Just parentBlock) tick benef validTxs newState
        receipts = map (uncurry $ mkReceipt newBlock) txOutputs
     in (newBlock, newState, receipts)

-- | Evaluate the transactions contained in the block against the given
-- state and return the new state and all the produced Receipts.
evalBlock
    :: forall c st tx o s.
       (Crypto.Hashable c tx)
    => Evaluator st tx o
    -> st
    -> Block c tx s
    -> (st, [Receipt c tx o])
evalBlock eval initialState blk =
    let (txOutputs, newState) = evalTraverse eval (blockTxs blk) initialState
        receipts = map (uncurry $ mkReceipt blk) txOutputs
     in (newState, toList receipts)

-- Internal ------------------------------------------------------

-- | Traverses transactions, evalutes them against the
-- given state and provides the output of the transaction.
--
-- If the evaluation of a transaction fails the state remains untouched
-- and we proceed evaluating the next transaction.
evalTraverse
    :: (Traversable t)
    => Evaluator state tx output
    -> t tx
    -> state
    -> (t (tx, Either EvalError output), state)
evalTraverse eval txs s = runState (traverse go txs) s
  where
    go tx = do
        result <- evalToState eval tx
        pure (tx, result)

evalToState :: Evaluator state tx output -> tx -> State state (Either EvalError output)
evalToState eval tx = state go
  where
    go st =
        case eval tx st of
            Left err            -> (Left err, st)
            Right (output, st') -> (Right output, st')


-- | Return an unsealed block holding the given transactions and state
-- on top of the given parent block.
mkUnsealedBlock
    :: forall c tx s st.
       ( Serialise tx
       , Serialise (Beneficiary c)
       , AuthTree.MerkleHash (Hash c)
       , Crypto.Hashable c (BlockHeader c Unsealed)
       , Crypto.Hashable c st
       )
    => Maybe (Block c tx s)
    -> Timestamp
    -> Beneficiary c
    -> [tx]
    -> st
    -> Block c tx Unsealed
mkUnsealedBlock parent blockTimestamp benef txs blockState =
    let (hdr :: BlockHeader c Unsealed) = emptyHeader
          { blockDataHash  = hashData $ mkBlockData benef txs
          , blockStateHash = Crypto.fromHashed (Crypto.hash blockState)
          , blockTimestamp
          }
    in mkBlock (header hdr) benef txs
  where
    header hdr = case parent of
        Nothing -> hdr -- We are creating 'genesis'
        Just p  -> hdr { blockHeight    = succ . blockHeight . blockHeader $ p
                       , blockPrevHash  = blockHash p
                       }
