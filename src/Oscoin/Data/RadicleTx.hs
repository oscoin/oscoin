-- | Provide Radicle transaction type, state and evaluator for the blockchain.
{-# LANGUAGE UndecidableInstances #-}
module Oscoin.Data.RadicleTx
    ( Env(..)
    , RadTx
    , Message
    , Output
    , RadEvaluator
    , txEval
    , pureEnv

    -- * Re-exports
    , Rad.Value
    , Tx(..)
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Eval (EvalError(..), Evaluator)
import           Oscoin.Crypto.Hash
import           Oscoin.Crypto.PubKey (PK, unsign)
import           Oscoin.Data.Query
import           Oscoin.Data.Tx (Tx(..))

import           Codec.Serialise (Serialise(..))
import           Data.Aeson (ToJSON, toJSON)
import           Data.Copointed (copoint)
import           Data.Default (Default(..))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Radicle
import qualified Radicle.Extended as Rad
import qualified Text.Show as Show

newtype Env c = Env { fromEnv :: Rad.Bindings (Rad.PrimFns Identity) }

instance Show.Show (Env c) where
    show = show . Rad.bindingsEnv . fromEnv

type Message = Rad.Value

type Output = Rad.Value

type RadTx c = Tx c Message

type RadEvaluator c = Evaluator (Env c) (RadTx c) Output

pureEnv :: Env c
pureEnv = Env Radicle.pureEnv

instance HasHashing c => Hashable c (Env c) where
    -- Nb. This instance is probably not correct, since the primops and refs
    -- are not being hashed. However, they are not currently hashable in
    -- radicle, so we only hash the Env for now.
    hash (Env bindings) =
        toHashed . fromHashed . hashSerial $ Radicle.bindingsEnv bindings

instance (ToJSON (Hash c), HasHashing c) => ToJSON (Env c) where
    toJSON e = toJSON (hash @c e)

instance Default (Env c) where
    def = pureEnv

instance Query (Env c) where
    type QueryVal (Env c) = Rad.Value

    query path (Env bindings) = do
        ident <- Rad.mkIdent (T.intercalate "/" path)
        val   <- Map.lookup ident (Rad.fromEnv $ Rad.bindingsEnv bindings)
        case copoint val of
            Rad.Ref ref -> lookupReference ref bindings
            val'        -> pure val'

data Program c = Program
    { progValue   :: Rad.Value
    , progAuthor  :: Hashed c (PK c)
    , progChainId :: Word16
    , progNonce   :: Word32
    } deriving (Generic)

deriving instance Show (Hash c) => Show (Program c)
instance Serialise (Hash c) => Serialise (Program c)

-- | Convert a 'Tx' to a Radicle 'Program'.
txToProgram :: Hashable c (PK c) => RadTx c -> Program c
txToProgram Tx{..} =
    Program
        { progValue   = unsign txMessage
        , progAuthor  = hash txPubKey
        , progChainId = txChainId
        , progNonce   = txNonce
        }

txEval :: Hashable c (PK c) => Evaluator (Env c) (RadTx c) Rad.Value
txEval tx st = radicleEval (txToProgram tx) st

radicleEval :: Evaluator (Env c) (Program c) Rad.Value
radicleEval Program{..} (Env st) =
    case runIdentity . Rad.runLang st $ Rad.eval progValue of
        (Left err, _)           -> Left (EvalError (show err))
        (Right value, newState) -> Right (value, Env newState)


lookupReference :: Rad.Reference -> Rad.Bindings m -> Maybe Rad.Value
lookupReference (Rad.Reference r) Rad.Bindings{..} =
    IntMap.lookup r bindingsRefs
