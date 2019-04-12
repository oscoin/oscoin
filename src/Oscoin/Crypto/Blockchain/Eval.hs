{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Crypto.Blockchain.Eval
    ( Evaluator
    , identityEval
    , EvalError(..)
    , EvalResult
    , Receipt(..)
    , buildBlock
    , buildBlockStrict
    , buildGenesis
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

identityEval :: Evaluator s tx ()
identityEval _ s = Right ((), s)

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
       , Crypto.Hashable c tx
       , Crypto.Hashable c st
       , Crypto.Hashable c (BlockHeader c Unsealed)
       , AuthTree.MerkleHash (Hash c)
       )
    => Evaluator st tx o
    -> Timestamp
    -> st
    -> [tx]
    -> BlockHash c
    -> (Block c tx Unsealed, st, [Receipt c tx o])
buildBlock eval tick st txs parentHash =
    let initialState = st
        (txOutputs, newState) = evalTraverse eval txs initialState
        validTxs = [tx | (tx, Right _) <- txOutputs]
        newBlock = mkUnsealedBlock parentHash tick validTxs newState
        receipts = map (uncurry $ mkReceipt newBlock) txOutputs
     in (newBlock, newState, receipts)


-- | Try to build a block like 'buildBlock' by applying @txs@ to the
-- state of a previous block. Unlinke 'buildBlock' evaluation will
-- abort if one of the transactions produces an error.
buildBlockStrict
    :: ( Serialise tx
       , Crypto.Hashable c st
       , Crypto.Hashable c (BlockHeader c Unsealed)
       , AuthTree.MerkleHash (Hash c)
       )
    => Evaluator st tx o
    -> Timestamp
    -> st
    -> [tx]
    -> BlockHash c
    -> Either (tx, EvalError) (Block c tx Unsealed)
buildBlockStrict eval tick st txs parentHash =
    mkEvaledUnsealedBlock
        eval
        tick
        txs
        parentHash
        st

-- | Try to build a genesis block. See 'buildBlockStrict' for more information.
buildGenesis
    :: ( Crypto.Hashable c st
       , Serialise tx
       , Crypto.Hashable c (BlockHeader c Unsealed)
       , AuthTree.MerkleHash (Hash c)
       )
    => Evaluator st tx o
    -> Timestamp
    -> [tx]
    -> st
    -> Either (tx, EvalError) (Block c tx Unsealed)
buildGenesis eval tick txs s =
    mkEvaledUnsealedBlock
        eval
        tick
        txs
        Crypto.zeroHash
        s

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
    let (txOutputs, newState) = evalTraverse eval (blockData blk) initialState
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
    :: forall c t tx st.
       ( Foldable t
       , Serialise tx
       , AuthTree.MerkleHash (Hash c)
       , Crypto.Hashable c (BlockHeader c Unsealed)
       , Crypto.Hashable c st
       )
    => BlockHash c -> Timestamp -> t tx -> st -> Block c tx Unsealed
mkUnsealedBlock parent blockTimestamp txs blockState =
    let (hdr :: BlockHeader c Unsealed) = emptyHeader
    in mkBlock (header hdr) txs
  where
    header hdr = hdr
        { blockPrevHash  = parent
        , blockDataHash  = hashTxs txs
        , blockStateHash = Crypto.fromHashed (Crypto.hash blockState)
        , blockTimestamp
        }

mkEvaledUnsealedBlock
    :: ( Serialise tx
       , Crypto.Hashable c st
       , Crypto.Hashable c (BlockHeader c Unsealed)
       , AuthTree.MerkleHash (Hash c)
       )
    => Evaluator st tx o
    -> Timestamp
    -> [tx]
    -> BlockHash c
    -> st
    -> Either (tx, EvalError) (Block c tx Unsealed)
mkEvaledUnsealedBlock eval tick txs parent initState =
    mkUnsealedBlock parent tick txs <$> foldlM step initState txs
  where
    step s tx =
        case eval tx s of
            Left err                  -> Left (tx, err)
            Right (_output, newState) -> Right newState
