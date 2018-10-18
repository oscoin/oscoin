module Oscoin.Crypto.Blockchain.Eval
    ( Evaluator
    , EvalError(..)
    , EvalResult
    , Receipt(..)
    , buildBlock
    , buildBlockStrict
    , buildGenesis
    , toOrphan
    , evalBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
import qualified Oscoin.Crypto.Hash as Crypto

import           Codec.Serialise (Serialise)
import           Data.Aeson
                 (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))


type EvalResult state output = Either EvalError (output, state)

type Evaluator state tx output = tx -> state -> EvalResult state output


newtype EvalError = EvalError { fromEvalError :: Text }
    deriving (Eq, Show, Read, Semigroup, Monoid, IsString, Generic)

instance Serialise EvalError

instance ToJSON EvalError where
    toJSON (EvalError e) = toJSON e

instance FromJSON EvalError where
    parseJSON v = EvalError <$> parseJSON v


-- | A 'Receipt' is generated whenever a transaction is evaluated as
-- part of a block.
data Receipt tx o = Receipt
    { receiptTx       :: Crypto.Hashed tx
    , receiptTxOutput :: Either EvalError o
    , receiptTxBlock  :: BlockHash
    -- ^ Identifies the block the output was generated in
    } deriving (Show, Eq, Generic, Functor)

mkReceipt :: (Crypto.Hashable tx) => Block tx s -> tx -> Either EvalError o -> Receipt tx o
mkReceipt block tx result = Receipt (Crypto.hash tx) result (blockHash block)

instance (Serialise tx, Serialise o) => Serialise (Receipt tx o)

instance (ToJSON tx, ToJSON o) => ToJSON (Receipt tx o) where
    toJSON Receipt{..} = object
        [ "txHash"      .= receiptTx
        , "txOutput"    .= receiptTxOutput
        , "txBlockHash" .= receiptTxBlock
        ]

instance (FromJSON tx, FromJSON o) => FromJSON (Receipt tx o) where
    parseJSON = withObject "Receipt" $ \o -> do
        receiptTx        <- o .: "txHash"
        receiptTxOutput  <- o .: "txOutput"
        receiptTxBlock   <- o .: "txBlockHash"
        pure Receipt{..}


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
        newBlock = mkUnsealedBlock (blockHash parent) tick validTxs newState
        receipts = map (uncurry $ mkReceipt newBlock) txOutputs
    in (newBlock, receipts)


-- | Try to build a block like 'buildBlock' by applying @txs@ to the
-- state of a previous block. Unlinke 'buildBlock' evaluation will
-- abort if one of the transactions produces an error.
buildBlockStrict
    :: (Serialise tx)
    => Evaluator s tx o
    -> Timestamp
    -> [tx]
    -> Block tx s
    -> Either (tx, EvalError) (Block tx s)
buildBlockStrict eval tick txs parent =
    mkEvaledUnsealedBlock
        eval
        tick
        txs
        (blockHash parent)
        (blockState $ blockHeader parent)

-- | Try to build a genesis block. See 'buildBlockStrict' for more information.
buildGenesis
    :: Serialise tx
    => Evaluator s tx o
    -> Timestamp
    -> [tx]
    -> s
    -> Either (tx, EvalError) (Block tx s)
buildGenesis eval tick txs s =
    mkEvaledUnsealedBlock
        eval
        tick
        txs
        (Crypto.toHashed Crypto.zeroHash)
        s

toOrphan :: Evaluator s tx a -> Block tx s' -> Block tx (Orphan s)
toOrphan eval blk =
    blk $> \s -> foldlM step s (blockData blk)
  where
    step s tx =
        case eval tx s of
            Left _        -> Nothing
            Right (_, s') -> Just s'

-- | Try to evaluate a block, given an initial state and evaluator.
evalBlock
    :: Evaluator s tx o
    -> s
    -> Block tx ()
    -> Either EvalError (Block tx s)
evalBlock eval st blk@Block{blockData} =
    map (blk $>) result
  where
    result = foldlM step st blockData
    step s tx = map snd (eval tx s)

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
mkUnsealedBlock :: (Foldable t, Serialise tx) => BlockHash -> Timestamp -> t tx -> s -> Block tx s
mkUnsealedBlock parent blockTimestamp txs blockState = mkBlock header txs
  where
    header =
         BlockHeader
            { blockPrevHash     = parent
            , blockDataHash     = hashTxs txs
            , blockState
            , blockDifficulty   = 0
            , blockTimestamp
            , blockNonce        = 0
            }

mkEvaledUnsealedBlock
    :: Serialise tx
    => Evaluator s tx o
    -> Timestamp
    -> [tx]
    -> BlockHash
    -> s
    -> Either (tx, EvalError) (Block tx s)
mkEvaledUnsealedBlock eval tick txs parent initState =
    mkUnsealedBlock parent tick txs <$> foldlM step initState txs
  where
    step s tx =
        case eval tx s of
            Left err                  -> Left (tx, err)
            Right (_output, newState) -> Right newState
