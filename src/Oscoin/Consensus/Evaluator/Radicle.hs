module Oscoin.Consensus.Evaluator.Radicle
    ( Env(..)
    , Program(..)
    , radicleEval
    , fromSource
    ) where

import           Oscoin.Prelude
import           Oscoin.Consensus.Evaluator (Evaluator)
import           Oscoin.Crypto.PubKey (PublicKey)
import           Oscoin.Crypto.Hash (Hashed, toHashed, zeroHash)
import           Oscoin.Data.Query

import qualified Radicle as Rad
import           Codec.Serialise (Serialise)
import qualified Data.Map as Map
import qualified Data.Text as T

newtype Env = Env { fromEnv :: Rad.Bindings Identity }

instance Default Env where
    def = Env Rad.Bindings
        { Rad.bindingsEnv = mempty
        , Rad.bindingsPrimops = mempty
        , Rad.bindingsRefs = mempty
        , Rad.bindingsNextRef = 0
        }

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

fromSource :: Text -> Text -> Either Text Program
fromSource name src = do
    values <- Rad.parse name src primops
    pure Program
        { progValue   = values
        , progAuthor  = toHashed zeroHash
        , progChainId = 0
        , progNonce   = 0
        }
  where
    primops = Map.keys $ Rad.bindingsPrimops $ Rad.pureEnv @Identity

-- | A radicle evaluator.
radicleEval :: Evaluator Env Program ()
radicleEval Program{..} (Env st) =
    case runIdentity . Rad.runLang st $ Rad.eval progValue of
        (Left _, _)  -> Nothing
        (Right _, s) -> Just ((), Env s)

