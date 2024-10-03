module Main (main) where

import Multiset
import Prelude

main :: IO ()
main = do
  let m1 = createMultiset :: Multiset Int
  print m1
  let m2 = insert (insert m1 1) 1
  print m2
  let m3 = delete m2 1
  print m3
  print $ find m3 1
