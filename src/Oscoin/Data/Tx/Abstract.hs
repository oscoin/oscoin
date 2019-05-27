-- | Types and type families used by all transaction types.
--
module Oscoin.Data.Tx.Abstract where

import           Oscoin.Prelude

-- | A transaction validation error.
type family TxValidationError c tx

-- | The output of applying a transaction to the state.
type family TxOutput c tx

-- | The input and output of transaction application.
type family TxState c tx

-- | The validation function used by for example the 'Mempool'.
type TxValidator c tx = tx -> Either (TxValidationError c tx) ()
