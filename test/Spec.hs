import qualified PropertyTestMultiset (properties)
import Test.Tasty
import qualified TestMultiset (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All tests"
    [ TestMultiset.tests,
      PropertyTestMultiset.properties
    ]
