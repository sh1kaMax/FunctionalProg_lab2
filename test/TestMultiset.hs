module TestMultiset (tests) where

import Multiset (Multiset (..), createMultiset, delete, filterMultiset, find, foldlMultiset, foldrMultiset, insert, mapMultiset)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Multiset unit-tests"
    [ testCase "Create multiset" testCreate,
      testCase "Insert in multiset" testInsert,
      testCase "Insert strings multiple" testInsertMultiple,
      testCase "Delete from multiset" testDelete,
      testCase "Delete strings multiple" testDeleteMultiple,
      testCase "Find True" testFindTrue,
      testCase "Find False" testFindFalse,
      testCase "Filter with even" testFilterEven,
      testCase "Filter witth lambda" testFilterLambda,
      testCase "Map with inc func" testMapInc,
      testCase "Map with strings" testMapString,
      testCase "Foldl with minus" testFoldlInt,
      testCase "Foldl with strings" testFoldlString,
      testCase "Foldr with minus" testFoldr
    ]

testCreate :: Assertion
testCreate = do
  let massive = (createMultiset :: Multiset Int)
  show massive @?= "[]"

testInsert :: Assertion
testInsert = do
  let massive = insert (insert (createMultiset :: Multiset Int) 1) 2
  show massive @?= "[1, 2]"

testInsertMultiple :: Assertion
testInsertMultiple = do
  let massive = insert (insert (insert (createMultiset :: Multiset String) "max") "max") "max"
  let massive1 = insert (insert (insert massive "itmo") "itmo") "itmo"
  show massive1 @?= "[\"max\", \"max\", \"max\", \"itmo\", \"itmo\", \"itmo\"]"

testDelete :: Assertion
testDelete = do
  let massive = insert (createMultiset :: Multiset Int) 1
  let massive1 = delete massive 1
  show massive1 @?= "[]"

testDeleteMultiple :: Assertion
testDeleteMultiple = do
  let massive = insert (insert (createMultiset :: Multiset String) "max") "max"
  let massive1 = insert (insert massive "itmo") "itmo"
  let massive2 = delete (delete massive1 "max") "itmo"
  show massive2 @?= "[\"max\", \"itmo\"]"

testFindTrue :: Assertion
testFindTrue = do
  let massive = insert (createMultiset :: Multiset Int) 1
  find massive 1 @?= True

testFindFalse :: Assertion
testFindFalse = do
  let massive = insert (createMultiset :: Multiset Int) 1
  find massive 2 @?= False

testFilterEven :: Assertion
testFilterEven = do
  let massive = insert (insert (insert (createMultiset :: Multiset Int) 1) 2) 3
  let filteredMassive = filterMultiset even massive
  show filteredMassive @?= "[2]"

testFilterLambda :: Assertion
testFilterLambda = do
  let massive = insert (insert (insert (createMultiset :: Multiset Int) 1) 2) 3
  let filteredMassive = filterMultiset (== 3) massive
  show filteredMassive @?= "[3]"

testMapInc :: Assertion
testMapInc = do
  let massive = insert (insert (insert (createMultiset :: Multiset Int) 1) 2) 3
  let mappedMassive = mapMultiset (+ 1) massive
  show mappedMassive @?= "[2, 3, 4]"

testMapString :: Assertion
testMapString = do
  let massive = insert (insert (insert (createMultiset :: Multiset String) "mam") "pap") "bab"
  let mappedMassive = mapMultiset (++ "a") massive
  show mappedMassive @?= "[\"mama\", \"papa\", \"baba\"]"

testFoldlInt :: Assertion
testFoldlInt = do
  let massive = insert (insert (insert (createMultiset :: Multiset Int) 1) 2) 3
  foldlMultiset (-) 6 massive @?= 0

testFoldlString :: Assertion
testFoldlString = do
  let massive = insert (insert (insert (createMultiset :: Multiset String) "a") "b") "c"
  foldlMultiset (++) "" massive @?= "abc"

testFoldr :: Assertion
testFoldr = do
  let massive = insert (insert (insert (createMultiset :: Multiset Int) 1) 2) 3
  foldrMultiset (-) 5 massive @?= -3
