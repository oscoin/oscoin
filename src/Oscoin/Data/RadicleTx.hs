-- | Provide Radicle transaction type, state and evaluator for the blockchain.
module Oscoin.Data.RadicleTx
    ( Env(..)
    , RadTx
    , RadEvaluator
    , txEval
    , pureEnv

    -- * Re-exports
    , Rad.Value
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Eval (EvalError(..), Evaluator)
import           Oscoin.Crypto.Hash (Hashed, hash)
import           Oscoin.Crypto.PubKey (PublicKey, unsign)
import           Oscoin.Data.Query
import           Oscoin.Data.Tx (Tx(..))

import           Codec.Serialise (Serialise)
import           Data.Default (Default(..))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Radicle
import qualified Radicle.Extended as Rad

newtype Env = Env { fromEnv :: Rad.Bindings (Rad.PrimFns Identity) }

type RadTx = Tx Rad.Value

type RadEvaluator = Evaluator Env RadTx Rad.Value

pureEnv :: Env
pureEnv = Env Radicle.pureEnv

instance Default Env where
    def = pureEnv

instance Query Env where
    type QueryVal Env = Rad.Value

    query path (Env bindings) = do
        ident <- Rad.mkIdent (T.intercalate "/" path)
        val <- Map.lookup ident (Rad.fromEnv $ Rad.bindingsEnv bindings)
        case val of
            Rad.Ref ref -> lookupReference ref bindings
            _           -> pure val

data Program = Program
    { progValue   :: Rad.Value
    , progAuthor  :: Hashed PublicKey
    , progChainId :: Word16
    , progNonce   :: Word32
    } deriving (Show, Generic)

instance Serialise Program

-- | Convert a 'Tx' to a Radicle 'Program'.
txToProgram :: RadTx -> Program
txToProgram Tx{..} =
    Program
        { progValue   = unsign txMessage
        , progAuthor  = hash txPubKey
        , progChainId = txChainId
        , progNonce   = txNonce
        }

txEval :: Evaluator Env RadTx Rad.Value
txEval tx st = radicleEval (txToProgram tx) st

radicleEval :: Evaluator Env Program Rad.Value
radicleEval Program{..} (Env st) =
    case runIdentity . Rad.runLang st $ Rad.eval progValue of
        (Left err, _)           -> Left (EvalError (show err))
        (Right value, newState) -> Right (value, Env newState)


lookupReference :: Rad.Reference -> Rad.Bindings m -> Maybe Rad.Value
lookupReference (Rad.Reference r) Rad.Bindings{..} =
    IntMap.lookup r bindingsRefs
