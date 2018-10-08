{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Data.Rad.Arbitrary
    (
    ) where

import           Oscoin.Prelude

import           Oscoin.Consensus.Evaluator.Radicle as Rad

import           Data.Either (fromRight)
import           Data.Functor.Identity (Identity(..))
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.Read (readMaybe)

import qualified Radicle.Extended as Rad
import           Radicle.Internal.Parse (isValidIdentFirst, isValidIdentRest)
import           Radicle.Internal.PrimFns (purePrimFns)

import           Radicle

data Leniency = Lenient | Strict
    deriving (Eq, Show, Ord, Enum, Bounded)

instance Arbitrary Rad.Env where
    arbitrary = pure Rad.pureEnv

instance Arbitrary r => Arbitrary (Radicle.Env r) where
    arbitrary = Radicle.Env <$> arbitrary

instance Arbitrary Value where
    arbitrary = Rad.tagDefault
              . Rad.untag
              . fromRight (Rad.List [])
              . Rad.parse "Arbitray"
              . Rad.prettyValue
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

        prims :: Rad.PrimFns Identity
        prims = purePrimFns

        isPrimop x = x `elem` Map.keys (getPrimFns prims)
        isNum x = isJust (readMaybe (toS $ fromIdent x) :: Maybe Scientific)

instance Arbitrary Rad.Ident where
    arbitrary = ((:) <$> firstL <*> rest) `suchThatMap` (mkIdent . toS)
      where
        allChars = take 100 ['!' .. maxBound]
        firstL = elements $ filter isValidIdentFirst allChars
        rest = sized $ \n -> do
            k <- choose (0, n)
            vectorOf k . elements $ filter isValidIdentRest allChars

instance Arbitrary a => Arbitrary (Rad.Bindings a) where
    arbitrary = do
        refs <- arbitrary
        env <- arbitrary
        prims <- arbitrary
        pure $ Rad.Bindings env prims (IntMap.fromList $ zip [0..] refs) (length refs)
