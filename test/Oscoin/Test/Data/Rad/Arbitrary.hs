{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Data.Rad.Arbitrary
    (
    ) where

import           Oscoin.Prelude

import           Oscoin.Data.RadicleTx as Rad.Tx
import qualified Radicle.Extended as RadX

import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Radicle
import qualified Radicle.Internal.Core as Radicle
import qualified Radicle.Internal.Doc as Radicle
import           Radicle.Internal.Identifier
                 (isValidIdentFirst, isValidIdentRest)

data Leniency = Lenient | Strict
    deriving (Eq, Show, Ord, Enum, Bounded)

instance Arbitrary (Rad.Tx.Env c) where
    arbitrary = pure Rad.Tx.pureEnv

instance Arbitrary r => Arbitrary (Radicle.Env r) where
    arbitrary = Radicle.Env . Map.map (Radicle.Docd Nothing) <$> arbitrary

-- | Generating a fully-fledged and diversified radicle 'Value' is going to
-- slow down tests considerably, therefore we stick to some trivial types.
instance Arbitrary Value where
    -- We have to 'tagDefault' to make sure there's no traces of the original
    -- parsed value in the AST annotations.
    arbitrary = RadX.tagDefault <$> frequency freqs
      where
        freqs = [ (20, Boolean <$> arbitrary)
                , (20, Number <$> arbitrary)
                , (60, Atom <$> arbitrary)
                ]

instance Arbitrary Radicle.Ident where
    arbitrary = ((:) <$> firstL <*> rest) `suchThatMap` (mkIdent . toS)
      where
        allChars = take 100 ['!' .. maxBound]
        firstL = elements $ filter isValidIdentFirst allChars
        rest = sized $ \n -> do
            k <- choose (0, n)
            vectorOf k . elements $ filter isValidIdentRest allChars

instance Arbitrary a => Arbitrary (Radicle.Bindings a) where
    arbitrary = do
        refs  <- arbitrary
        env   <- arbitrary
        prims <- arbitrary
        pure Radicle.emptyBindings
            { Radicle.bindingsEnv     = env
            , Radicle.bindingsPrimFns = prims
            , Radicle.bindingsRefs    = IntMap.fromList $ zip [0..] refs
            , Radicle.bindingsNextRef = length refs
            }
