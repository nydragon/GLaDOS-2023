module Parsing where

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

parsingTests :: TestTree
parsingTests = testGroup "Parsing Tests" [
        testCase "first" $
            2+2 @?= 4
    ]
