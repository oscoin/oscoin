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

import qualified Radicle as Rad
import           Codec.Serialise (Serialise, serialise, deserialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map

newtype Env = Env { fromEnv :: Rad.Bindings Identity }

instance Default Env where
    def = Env Rad.Bindings
        { Rad.bindingsEnv = mempty
        , Rad.bindingsPrimops = mempty
        , Rad.bindingsRefs = mempty
        , Rad.bindingsNextRef = 0
        }

data Program = Program
    { progSource  :: ByteString
    , progAuthor  :: Hashed PublicKey
    , progChainId :: Word16
    , progNonce   :: Word32
    } deriving (Show, Generic)

instance Serialise Program

fromSource :: Text -> Text -> Either Text Program
fromSource name src = do
    values <- parse
    pure Program
        { progSource  = LBS.toStrict (serialise values)
        , progAuthor  = toHashed zeroHash
        , progChainId = 0
        , progNonce   = 0
        }
  where
    parse   = Rad.parse name src primops
    primops = Map.keys $ Rad.bindingsPrimops $ Rad.pureEnv @Identity

-- | A radicle evaluator.
radicleEval :: Evaluator Env Program ()
radicleEval Program{..} (Env st) =
    case runIdentity . Rad.runLang st $ Rad.eval value of
        (Left _, _)  -> Nothing
        (Right _, s) -> Just ((), Env s)
  where
    value = deserialise (LBS.fromStrict progSource)

