module Test.Parsing.Args where

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

testLambda :: TestTree
testLambda =  testCase "Exemple" $ do
    2 + 2 @?= 4