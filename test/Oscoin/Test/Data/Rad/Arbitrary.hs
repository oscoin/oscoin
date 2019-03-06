{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Data.Rad.Arbitrary
    (
    ) where

import           Oscoin.Prelude

import           Oscoin.Data.RadicleTx as Rad.Tx
import qualified Radicle.Extended as RadX

import           Data.Either (fromRight)
import           Data.Functor.Identity (Identity(..))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.Read (readMaybe)

import           Radicle
import qualified Radicle.Internal.Core as Radicle
import qualified Radicle.Internal.Doc as Radicle
import           Radicle.Internal.Identifier
                 (isValidIdentFirst, isValidIdentRest)
import           Radicle.Internal.PrimFns (purePrimFns)

data Leniency = Lenient | Strict
    deriving (Eq, Show, Ord, Enum, Bounded)

instance Arbitrary (Rad.Tx.Env c) where
    arbitrary = pure Rad.Tx.pureEnv

instance Arbitrary r => Arbitrary (Radicle.Env r) where
    arbitrary = Radicle.Env . Map.map (Radicle.Docd Nothing) <$> arbitrary

instance Arbitrary Value where
    -- We have to untag/tagDefault to make sure there's no traces of the original
    -- parsed value in the AST annotations.
    arbitrary = RadX.tagDefault
              . Radicle.untag
              . fromRight (Radicle.List [])
              . Radicle.parse "Arbitray"
              . RadX.prettyValue
            <$> sized go
      where
        -- There's no literal syntax for dicts, only the 'dict' primop. If we
        -- generated them directly, we would generate something that can only
        -- be got at after an eval, and which doesn't really correspond to
        -- anything a user can write. So we don't generate dicts directly,
        -- instead requiring they go via the primop.
        freqs = [ (3, Atom <$> (arbitrary `suchThat` (\x -> not (isPrimop x || isNum x))))
                , (3, Boolean <$> arbitrary)
                , (3, Number <$> arbitrary)
                , (1, List <$> sizedList)
                , (6, PrimFn <$> elements (Map.keys $ getPrimFns prims))
                , (1, Lambda <$> sizedList
                             <*> scale (`div` 3) arbitrary
                             <*> scale (`div` 3) arbitrary)
                ]
        go n | n == 0 = frequency $ first pred <$> freqs
             | otherwise = frequency freqs

        sizedList :: Arbitrary a => Gen [a]
        sizedList = sized $ \n -> do
            k <- choose (0, n)
            scale (`div` (k + 1)) $ vectorOf k arbitrary

        prims :: Radicle.PrimFns Identity
        prims = purePrimFns

        isPrimop x = x `elem` Map.keys (getPrimFns prims)
        isNum x = isJust (readMaybe (toS $ fromIdent x) :: Maybe Scientific)

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
