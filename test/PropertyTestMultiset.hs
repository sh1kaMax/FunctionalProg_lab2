{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
module PropertyTestMultiset (properties) where

import Multiset
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

properties :: TestTree
properties =
  testGroup
    "Multiset properties-tests"
    [ QC.testProperty "Test monoid property" testMempty,
      QC.testProperty "Test find func" testFind,
      QC.testProperty "Test filter with always True" testFilter,
      QC.testProperty "Test mconcat" testMconcat
    ]

testMempty :: Multiset Int -> Bool
testMempty m = (m <> mempty) == m && (mempty <> m) == m

testFind :: Multiset Int -> Int -> Bool
testFind m v = find new_m v
  where
    new_m = insert m v

testFilter :: Multiset Int -> Bool
testFilter m = filterMultiset (const True) m == m

testMconcat :: Multiset Int -> Multiset Int -> Bool
testMconcat m1 m2 = m1 <> m2 == mconcat [m1, m2]
