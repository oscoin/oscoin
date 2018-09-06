{-# OPTIONS_GHC -fno-warn-orphans #-}
module Oscoin.Test.Data.Rad.Arbitrary where

import           Oscoin.Prelude

import           Data.Functor.Identity (Identity(..))
import           Text.Read (readMaybe)
import qualified Data.Map as Map
import           Data.Text (pack, unpack)
import           Data.Scientific (Scientific)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Radicle
import           Radicle.Internal.Parse (isValidIdentFirst, isValidIdentRest)
import           Radicle.Internal.Primops (purePrimops)

data Leniency = Lenient | Strict
    deriving (Eq, Show, Ord, Enum, Bounded)

instance Arbitrary r => Arbitrary (Env r) where
    arbitrary = Env <$> arbitrary

instance Arbitrary Value where
    arbitrary =
        sized go
      where
        -- There's no literal syntax for dicts, only the 'dict' primop. If we
        -- generated them directly, we would generate something that can only
        -- be got at after an eval, and which doesn't really correspond to
        -- anything a user can write. So we don't generate dicts directly,
        -- instead requiring they go via the primop.
        freqs = [ (3, Atom <$> (arbitrary `suchThat` (\x -> not (isPrimop x || isNum x))))
                , (3, String <$> arbitrary)
                , (3, Boolean <$> arbitrary)
                , (3, Number <$> arbitrary)
                , (1, List <$> sizedList)
                , (6, Primop <$> elements (Map.keys prims))
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

        prims :: Map.Map Ident ([Value] -> Lang Identity Value)
        prims = purePrimops

        isPrimop x = x `elem` Map.keys prims

        isNum x = isJust (readMaybe (unpack $ fromIdent x) :: Maybe Scientific)

instance Arbitrary Ident where
    arbitrary = ((:) <$> firstL <*> rest) `suchThatMap` (mkIdent . pack)
      where
        allChars = take 100 ['!' .. maxBound]
        firstL = elements $ filter isValidIdentFirst allChars
        rest = sized $ \n -> do
            k <- choose (0, n)
            vectorOf k . elements $ filter isValidIdentRest allChars
