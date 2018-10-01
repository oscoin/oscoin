module Oscoin.Consensus.Evaluator.Radicle
    ( Env(..)
    , Program(..)
    , radicleEval
    , parseValue
    , fromSource

    -- * Re-exports
    , Rad.Value
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Evaluator (EvalError(..), Evaluator)
import           Oscoin.Crypto.Hash (Hashed, toHashed, zeroHash)
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Data.Query

import           Codec.Serialise (Serialise)
import           Data.Default (Default(..))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Radicle.Extended as Rad

newtype Env = Env { fromEnv :: Rad.Bindings (Rad.PrimFns Identity) }

instance Default Env where
    def = Env Rad.pureEnv

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

parseValue :: Text -> Text -> Either Text Rad.Value
parseValue name src = Rad.parse name src

fromSource :: Text -> Text -> Either Text Program
fromSource name src = do
    value <- parseValue name src
    pure Program
        { progValue   = value
        , progAuthor  = toHashed zeroHash
        , progChainId = 0
        , progNonce   = 0
        }

-- | A radicle evaluator.
radicleEval :: Evaluator Env Program Rad.Value
radicleEval Program{..} (Env st) =
    case runIdentity . Rad.runLang st $ Rad.eval progValue of
        (Left err, _)           -> Left [EvalError (show err)]
        (Right value, newState) -> Right (value, Env newState)

lookupReference :: Rad.Reference -> Rad.Bindings m -> Maybe Rad.Value
lookupReference (Rad.Reference r) Rad.Bindings{..} =
    IntMap.lookup r bindingsRefs
