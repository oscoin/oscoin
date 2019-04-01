module Test.Data.Sequence.Circular
    ( tests
    ) where

import           Prelude hiding (sequence)

import           Data.Sequence.Circular as Seq

import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (elements)

tests :: TestTree
tests =
    testGroup "Data.Sequence.Circular"
        [ testProperty "behaves correctly" worksOKProp ]

{------------------------------------------------------------------------------
  The tests proper
------------------------------------------------------------------------------}

worksOKProp :: Positive Int -> [Int] -> Property
worksOKProp (fromIntegral . getPositive -> size) elements = do
    --  Note that due to the use of 'foldr', the elements are folded
    -- "right to left".
    -- >>> foldr (Seq.<|) (Seq.empty 3) [1,2,3,4,5]
    -- [1,2,3]
    let sequence = foldr (Seq.<|) (Seq.empty size) elements
    -- Property which must hold:
    -- 1. If `length elements < size`, then `toList sequence == elements`
    -- 2. else `toList sequence == take size elements`
    if length elements < fromIntegral size
       then Seq.toList sequence === elements
       else Seq.toList sequence === take (fromIntegral size) elements
