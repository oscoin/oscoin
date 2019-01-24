module Oscoin.Telemetry.Logging.Generic
    ( gderiveErrorClass
    ) where

import           Oscoin.Prelude

import           Data.Char (isLower, isUpper, toLower)
import           Data.List (groupBy)
import           Generics.SOP (NP(..), SOP(..))
import qualified Generics.SOP as SOP

-- | Derives the \"error_class\" tag automatically via the constructor names
-- of the input value.
gderiveErrorClass :: forall a. (SOP.Generic a, SOP.HasDatatypeInfo a)
                  => a
                  -> Text
gderiveErrorClass = gderive (SOP.datatypeInfo (Proxy :: Proxy a)) . SOP.from
  where
      gderive :: SOP.SListI xss => SOP.DatatypeInfo xss -> SOP.SOP SOP.I xss -> Text
      gderive dInfo (SOP sop)    =
          let constructorInfo = case dInfo of
                  SOP.ADT _ _ cs    -> cs
                  SOP.Newtype _ _ c -> c :* Nil
          in SOP.hcollapse $ SOP.hliftA2 deriveSnakeCase constructorInfo sop

      deriveSnakeCase :: SOP.ConstructorInfo xs -> SOP.NP SOP.I xs -> SOP.K Text xs
      deriveSnakeCase cInfo _ =
          let cName = case cInfo of
                  SOP.Constructor n -> n
                  SOP.Infix n _ _   -> n
                  SOP.Record n _    -> n
          in SOP.K . toS . toSnakeCase $ cName

      -- Turns a CamelCase constructor into a snake_case one.
      -- >>> InvalidBlockSize
      -- invalid_block_size
      --
      toSnakeCase :: String -> String
      toSnakeCase = map toLower     -- "invalid_block_size"
                  . mconcat         -- "Invalid_Block_Size"
                  . intersperse "_" -- ["Invalid", "_", "Block", "_", "Size"]
                  . groupBy (\x y -> isUpper x && isLower y) -- ["Invalid", "Block", "Size"]


