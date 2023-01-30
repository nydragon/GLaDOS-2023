module TokenizationTest where

import Test.Tasty
import Test.Tasty.HUnit

test1 :: TestTree
test1 = testCase "Test 1" $ 2 + 2 @?= 4

-- Test suite list
tokenSuite :: TestTree
tokenSuite = testGroup "Tokenization Test Suite" [test1]