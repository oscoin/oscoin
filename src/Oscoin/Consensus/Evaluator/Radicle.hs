module Oscoin.Consensus.Evaluator.Radicle
    ( Env(..)
    , Program(..)
    , radicleEval
    , parseValue
    , fromSource

    -- * Re-exports
    , Rad.Value
    ) where

import           Oscoin.Consensus.Evaluator (Evaluator, evalError)
import           Oscoin.Crypto.Hash (Hashed, toHashed, zeroHash)
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Data.Query
import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import           Data.Default (Default(..))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Radicle as Rad

newtype Env = Env { fromEnv :: Rad.Bindings Identity }

instance Default Env where
    def = Env Rad.pureEnv

instance Query Env where
    type QueryVal Env = Rad.Value

    query path (Env bindings) =
        case Rad.mkIdent (T.intercalate "/" path) of
            Just ident ->
                Map.lookup ident (Rad.fromEnv $ Rad.bindingsEnv bindings)
            Nothing ->
                Nothing

data Program = Program
    { progValue   :: Rad.Value
    , progAuthor  :: Hashed PublicKey
    , progChainId :: Word16
    , progNonce   :: Word32
    } deriving (Show, Generic)

instance Serialise Program

parseValue :: Text -> Text -> Either Text Rad.Value
parseValue name src =
    Rad.parse name src primops
  where
    primops = Map.keys $ Rad.bindingsPrimops $ Rad.pureEnv @Identity

fromSource :: Text -> Text -> Either Text Program
fromSource name src = do
    value <- parseValue name src
    pure Program
        { progValue  = value
        , progAuthor  = toHashed zeroHash
        , progChainId = 0
        , progNonce   = 0
        }

-- | A radicle evaluator.
radicleEval :: Evaluator Env Program ()
radicleEval Program{..} (Env st) =
    case runIdentity . Rad.runLang st $ Rad.eval progValue of
        (Left err, _) -> Left [evalError (show err)]
        (Right _, s)  -> Right ((), Env s)

